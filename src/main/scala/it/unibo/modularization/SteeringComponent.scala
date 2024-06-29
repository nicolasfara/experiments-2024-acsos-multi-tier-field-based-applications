package it.unibo.modularization

import it.unibo.alchemist.model.Position
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class SteeringComponent extends MyAggregateProgram {

  override def main(): Any = {
    val isRescuer = senseOr[Boolean]("isRescuer", false)
    val isLeader = senseOr[Boolean]("isRegionLeader", false)
    val nodeRequiredIntervention = inputFromComponent("nodeRequiredIntervention", Option.empty[(ID, Position[_])])

    mux(nodeRequiredIntervention.isDefined) {
      val (nodeId, _) = nodeRequiredIntervention.get
      val potential = classicGradient(isLeader, nbrRange _) // This can be improved by reading the potential value from the environment...
      val distanceToEmergency = alchemistEnvironment.getDistanceBetweenNodes(alchemistEnvironment.getNodeByID(nodeId), alchemistEnvironment.getNodeByID(mid()))
      val rescuersInRegion = C[Double, Set[(ID, Double)]](
        potential,
        _ ++ _,
        mux(isRescuer){ Set(mid() -> distanceToEmergency) }{ Set.empty[(ID, Double)] },
        Set(),
      )
      val candidate = rescuersInRegion.minByOption(_._2)
      mux(candidate.isDefined) {
        val rescuerId = candidate.get._1
        writeEnv("rescuerId", rescuerId)
      } {
        // No candidate found
      }
    } {
      // Do nothing here, no intervention required
    }
  }
}
