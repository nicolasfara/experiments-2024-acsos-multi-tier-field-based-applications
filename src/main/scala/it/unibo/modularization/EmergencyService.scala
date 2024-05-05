package it.unibo.modularization

class EmergencyService extends MyAggregateProgram {

  override def main() = {
    val hopCount = classicGradient(mid() == 9)
    writeEnv("potential", hopCount)
    hopCount
  }
}
