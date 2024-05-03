package it.unibo.modularization

class OtherComponent extends MyAggregateProgram {

  override def main(): Any = {
    val input = inputFromComponent("it.unibo.modularization.EmergencyService", Double.PositiveInfinity)
    writeEnv("InputFromEmergency", input)
  }
}
