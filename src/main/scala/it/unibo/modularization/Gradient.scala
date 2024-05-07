package it.unibo.modularization

class Gradient extends MyAggregateProgram {

  override def main(): Double = {
    val potential = classicGradient(mid() == 10, nbrRange _)
    writeEnv("potential", potential)
    potential
//    val isRegionLeader = S(2500.0, nbrRange _)
//    writeEnv("isRegionLeader", isRegionLeader)
//    val distanceFromLeader = classicGradient(isRegionLeader, nbrRange _)
//    writeEnv("distanceFromLeader", distanceFromLeader)
//    distanceFromLeader
  }
}
