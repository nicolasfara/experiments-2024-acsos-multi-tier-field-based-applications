package it.unibo.modularization

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins.Bounded

private case class Candidacy(symBreaker: Int, distance: Double, leaderId: Int)

private object BoundedMessage {
  implicit object BoundedMsg extends Bounded[Candidacy] {
    override def bottom: Candidacy = Candidacy(implicitly[Bounded[Int]].bottom, implicitly[Bounded[Double]].bottom, implicitly[Bounded[ID]].bottom)

    override def top: Candidacy = Candidacy(implicitly[Bounded[Int]].top, implicitly[Bounded[Double]].top, implicitly[Bounded[ID]].top)

    override def compare(a: Candidacy, b: Candidacy): Int =
      List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.leaderId.compareTo(b.leaderId)).collectFirst { case x if x != 0 => x }.getOrElse(0)
  }
}

class LeaderElection extends MyAggregateProgram {
  override def main(): Any = {
//    val symBreaker = branch(alchemistTimestamp.toDouble.toLong % 1200 < 600) { mid() } {
//      rep(alchemistRandomGen.nextInt())(identity)
//    }
    val symBreaker = mid()
    val isRegionLeader = SWithMinimisingShare(2500.0, symBreaker) == mid()
    writeEnv("isRegionLeader", isRegionLeader)
    val regionPotential = classicGradient(isRegionLeader, nbrRange _)
    val leaderId = G[ID](isRegionLeader, mid(), identity, nbrRange _)
    writeEnv("potential", regionPotential)
    writeEnv("leaderId", leaderId)
    regionPotential
  }

  import BoundedMessage._

  // cf. https://arxiv.org/pdf/1711.08297.pdf
  def SWithMinimisingShare(grain: Double, symBreaker: Int): ID = {
     def fMP(value: Candidacy): Candidacy = value match {
      case Candidacy(_, dd, id) if id == mid() || dd >= grain => implicitly[Bounded[Candidacy]].top
      case m => m
    }

    val loc = Candidacy(symBreaker, 0.0, mid())
    share[Candidacy](loc) { case (_, nbrc) =>
      minHoodPlusLoc(loc){
        val nbrCandidacy = nbrc()
        fMP(nbrCandidacy.copy(distance = nbrCandidacy.distance + nbrRange()))
      }
    }.leaderId
  }
}
