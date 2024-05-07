package it.unibo.alchemist.model

import it.unibo.alchemist.model.molecules.SimpleMolecule
import org.apache.commons.math3.random.RandomGenerator

class PowerModelProperty[T, P <: Position[P]](
    private val environment: Environment[T, P],
    private val node: Node[T],
    private val random: RandomGenerator,
    private val deviceEnergyPerInstruction: Double, // mJ
    private val componentInstructionsCount: Int,
    private val batteryCapacity: Double = 4000.0 // mAh
) extends NodeProperty[T] {

  private val osInstructionsCount = 20e6
  private val networkInstructionsCount = 100e6

  private var previousTime = 0.0
  private var currentBatteryLevel = batteryCapacity
  private var messagesProcessedSinceLastUpdate = 0

  def updatePowerModelForNode(): Unit = {
    val deltaTime = currentSimulationTime - previousTime
    previousTime = currentSimulationTime
    dischargeBattery(deltaTime)
    messagesProcessedSinceLastUpdate = 0
  }

  private def dischargeBattery(deltaTime: Double): Unit = {
    val joulesConsumedByComponent = deviceEnergyPerInstruction * componentInstructionsCount
    val joulesConsumedByOs = deviceEnergyPerInstruction * (osInstructionsCount * random.nextGaussian())
    val joulesConsumedByNetwork = deviceEnergyPerInstruction * networkInstructionsCount * messagesProcessedSinceLastUpdate

    val consumedWatt = toWatt(joulesConsumedByComponent + joulesConsumedByOs + joulesConsumedByNetwork, deltaTime)
    // Supposing a smartphone battery voltage of 3.7V
    val consumedMilliAmpsInDelta = toMilliAmps(consumedWatt, 3.7)
    if (currentBatteryLevel - consumedMilliAmpsInDelta < 0) {
      currentBatteryLevel = 0
      node.setConcentration(new SimpleMolecule("BatteryLevel"), currentBatteryLevel.asInstanceOf[T])
      return
    }
    currentBatteryLevel -= consumedMilliAmpsInDelta
    node.setConcentration(new SimpleMolecule("BatteryLevel"), currentBatteryLevel.asInstanceOf[T])
  }

  def processNewMessage(): Unit = messagesProcessedSinceLastUpdate += 1

  private def toWatt(joules: Double, delta: Double): Double = joules / delta

  private def toMilliAmps(watt: Double, volt: Double): Double = watt / volt * 1000

  private def currentSimulationTime = environment.getSimulation.getTime.toDouble

  override def getNode: Node[T] = node

  override def cloneOnNewNode(node: Node[T]): NodeProperty[T] = ???
}
