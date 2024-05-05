package it.unibo.modularization

class EmergencyService extends MyAggregateProgram {

  override def main(): Double = {
    val isRegionLeader = S(2500.0, nbrRange _)
    writeEnv("isRegionLeader", isRegionLeader)
    val distanceFromLeader = classicGradient(isRegionLeader, nbrRange _)
    writeEnv("distanceFromLeader", distanceFromLeader)
    distanceFromLeader
  }
}
