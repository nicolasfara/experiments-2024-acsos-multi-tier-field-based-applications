package it.unibo.modularization

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins.Bounded

private case class Candidacy(symBreaker: Int, distance: Double, leaderId: Int)

private object BoundedMessage {
  implicit object BoundedMsg extends Bounded[Candidacy] {
    override def bottom: Candidacy = Candidacy(implicitly[Bounded[Int]].bottom, implicitly[Bounded[Double]].bottom, implicitly[Bounded[ID]].bottom)

    override def top: Candidacy = Candidacy(implicitly[Bounded[Int]].top, implicitly[Bounded[Double]].top, implicitly[Bounded[ID]].top)

    override def compare(a: Candidacy, b: Candidacy): Int =
      List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.leaderId.compareTo(b.leaderId))
        .collectFirst { case x if x != 0 => x }
        .getOrElse(0)
  }
}

class EmergencyComponent extends MyAggregateProgram {

  override def main(): Any = {
    val isLeader = SWithMinimisingShare(2500.0, mid(), nbrRange _) == mid()
    writeEnv("isRegionLeader", isLeader)
    val regionPotential = classicGradient(isLeader, nbrRange _)
    val leaderId = G[ID](isLeader, mid(), identity, nbrRange _)
    writeEnv("leaderId", leaderId)
    val collectData = C[Double, Map[ID, Double]](regionPotential, _ ++ _, Map(mid() -> randomGenerator().nextGaussian()), Map())
    val nodeRequiredInterventionLocal = checkForEmergency(collectData)
    val nodeRequiredIntervention = G[Option[ID]](isLeader, nodeRequiredInterventionLocal, identity, nbrRange _)
    writeEnv("nodeRequiredIntervention", nodeRequiredIntervention)
    nodeRequiredIntervention
  }

  private def checkForEmergency(regionData: Map[ID, Double]): Option[ID] = regionData.toList
    .sortBy(_._2)
    .find(_._2 > 0.9)
    .map(_._1)

  import BoundedMessage._

  // cf. https://arxiv.org/pdf/1711.08297.pdf
  def SWithMinimisingShare(grain: Double, symBreaker: Int, metric: Metric): ID = {
    def fMP(value: Candidacy): Candidacy = value match {
      case Candidacy(_, dd, id) if id == mid() || dd >= grain => implicitly[Bounded[Candidacy]].top
      case m                                                  => m
    }

    val loc = Candidacy(symBreaker, 0.0, mid())
    share[Candidacy](loc) { case (_, nbrc) =>
      minHoodPlusLoc(loc) {
        val nbrCandidacy = nbrc()
        fMP(nbrCandidacy.copy(distance = nbrCandidacy.distance + metric()))
      }
    }.leaderId
  }
}
