package it.unibo.modularization

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class MonolithProgram extends MyAggregateProgram {
  override def main(): Any = {
    val isRegionLeader = S(2500.0, nbrRange _)
    val distanceFromLeader = classicGradient(isRegionLeader, nbrRange _)
    val nodeInRegion = C[Double, Set[ID]](distanceFromLeader, _ ++ _, Set(mid()), Set[ID]())
    writeEnv("isRegionLeader", isRegionLeader)
    writeEnv("distanceFromLeader", distanceFromLeader)
    nodeInRegion
  }
}
