package it.unibo.modularization

class EmergencyService extends MyAggregateProgram {

  override def main() = {
    val hopCount = classicGradient(mid() == 8)
    writeEnv("potential", hopCount)
    hopCount
  }
}
