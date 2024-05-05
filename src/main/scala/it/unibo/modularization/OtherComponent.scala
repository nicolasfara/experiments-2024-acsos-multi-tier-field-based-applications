package it.unibo.modularization

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class OtherComponent extends MyAggregateProgram {

  override def main(): Any = {
    val input = inputFromComponent("it.unibo.modularization.EmergencyService", Double.PositiveInfinity)
    val nodeInRegion = C[Double, Set[ID]](input, _ ++ _, Set(mid()), Set[ID]())
    writeEnv("InputFromEmergency", input)
    writeEnv("NodeInRegion", nodeInRegion)
  }
}
