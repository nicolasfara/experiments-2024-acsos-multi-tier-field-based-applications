package it.unibo.alchemist.model

import it.unibo.alchemist.model.molecules.SimpleMolecule
import org.apache.commons.math3.random.RandomGenerator

import scala.jdk.CollectionConverters.CollectionHasAsScala

class PowerModelProperty[T, P <: Position[P]](
    private val environment: Environment[T, P],
    private val node: Node[T],
    private val random: RandomGenerator,
    private val deviceEnergyPerInstruction: Double, // nJ
    private val componentInstructionsCount: Int,
    private val batteryCapacity: Double = 4000.0, // mAh
) extends NodeProperty[T] {

  private lazy val allocator = node.getProperties
    .asScala
    .filter(_.isInstanceOf[AllocatorProperty[T, P]])
    .map(_.asInstanceOf[AllocatorProperty[T, P]])
    .head

  private val osInstructionsCount = 1
  private val networkInstructionsCount = 3

  private var previousTime = 0.0
  private var currentBatteryLevel = batteryCapacity * random.nextDouble()
  private var messagesProcessedSinceLastUpdate = 0

  def getBatteryLevel: Double = currentBatteryLevel

  def updatePowerModelForNode(): Unit = {
    val deltaTime = currentSimulationTime - previousTime
    previousTime = currentSimulationTime
    dischargeBattery(deltaTime)
    messagesProcessedSinceLastUpdate = 0
  }

  private def dischargeBattery(deltaTime: Double): Unit = {
    val joulesConsumedByComponent = deviceEnergyPerInstruction * componentInstructionsCount * 1e-3
    val joulesConsumedByOs = deviceEnergyPerInstruction * (osInstructionsCount * random.nextDouble()) * 1e-3
    val joulesConsumedByNetwork = deviceEnergyPerInstruction * networkInstructionsCount * messagesProcessedSinceLastUpdate * 1e-3

    val localComponents = allocator.getComponentsAllocation.count(_._2 == LocalNode)

    val consumedWatt = toWatt(joulesConsumedByComponent * localComponents + joulesConsumedByOs + joulesConsumedByNetwork, deltaTime)
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
