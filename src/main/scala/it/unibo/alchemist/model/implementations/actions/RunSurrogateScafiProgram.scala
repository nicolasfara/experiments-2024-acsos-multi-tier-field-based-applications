package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model._
import it.unibo.alchemist.model.actions._
import it.unibo.alchemist.model.implementations.actions.RunScafiProgram.NeighborData
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.scala.PimpMyAlchemist._
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath
import org.kaikikm.threadresloader.ResourceLoader

import scala.jdk.CollectionConverters.{IterableHasAsScala, IteratorHasAsScala}
import scala.util.{Failure, Try}

sealed class RunSurrogateScafiProgram[T, P <: Position[P]](
    environment: Environment[T, P],
    node: Node[T],
    reaction: Reaction[T],
    randomGenerator: RandomGenerator,
    programName: String,
    retentionTime: Double,
    programDagMapping: Map[String, List[String]] = Map.empty
) extends AbstractLocalAction[T](node) {

  private val targetMolecule = new SimpleMolecule("Target")

  def this(
      environment: Environment[T, P],
      node: Node[T],
      reaction: Reaction[T],
      randomGenerator: RandomGenerator,
      programName: String
  ) = this(environment, node, reaction, randomGenerator, programName, FastMath.nextUp(1.0 / reaction.getTimeDistribution.getRate))

  private var completed = false
  declareDependencyTo(Dependency.EVERY_MOLECULE)

  val program = ResourceLoader
    .classForName(programName)
    .getDeclaredConstructor()
    .newInstance()
    .asInstanceOf[CONTEXT => EXPORT]
  val programDag = programDagMapping
  val asMolecule = new SimpleMolecule(programName)
  private val surrogateForNodes = collection.mutable.Set[ID]()
  private val contextManager = collection.mutable.Map[ID, CONTEXT]()
  // Map of node ID to a map of neighbor ID to NeighborData
  private val neighborhoodManager = collection.mutable.Map[ID, collection.mutable.Map[ID, NeighborData[P]]]()
  private val currentNeighborhoodOfNodes = collection.mutable.Map[ID, Set[ID]]()

  override def cloneAction(node: Node[T], reaction: Reaction[T]): Action[T] =
    new RunSurrogateScafiProgram[T, P](environment, node, reaction, randomGenerator, programName, retentionTime)

  override def execute(): Unit = {
    val alchemistCurrentTime = Try(environment.getSimulation)
      .map(_.getTime)
      .orElse(Failure(new IllegalStateException("The simulation is uninitialized (did you serialize the environment?)")))
      .get

    // Clean the neighborhood manager according to the retention time
    neighborhoodManager.foreach { case (_, data) =>
      data.filterInPlace((_, p) => p.executionTime >= alchemistCurrentTime - retentionTime)
    }
    // Clean the surrogateForNodes according to the retention time
    val applicativeDevice = activeApplicationNeighborDevices.map(_.getId)
    surrogateForNodes.filterInPlace(nodeId => applicativeDevice.contains(nodeId))
    currentNeighborhoodOfNodes.filterInPlace((nodeId, _) => applicativeDevice.contains(nodeId))

    // Run the program for each node offloading the computation to this surrogate
    surrogateForNodes.foreach(deviceId => {
      contextManager.get(deviceId) match {
        case Some(contextNode) =>
          val computedResult = program(contextNode)
          val nodePosition = environment.getPosition(environment.getNodeByID(deviceId))
          val toSend = NeighborData(computedResult, nodePosition, alchemistCurrentTime)
          val neighborsToSend = currentNeighborhoodOfNodes(deviceId)
            .map(neighId => neighId -> toSend)
            .to(collection.mutable.Map) ++ Map(deviceId -> toSend)
          neighborhoodManager.put(deviceId, neighborsToSend)
        case None => ()
      }
    })
    val resultsForEachComputedNode = neighborhoodManager.view.mapValues(_.view.mapValues(_.exportData.root[T]()).toMap).toMap
    node.setConcentration(asMolecule, resultsForEachComputedNode.asInstanceOf[T])
    node.setConcentration(new SimpleMolecule("SurrogateFor"), isSurrogateFor.asInstanceOf[T])
    completed = true
  }

  private def activeApplicationNeighborDevices: List[Node[T]] = {
    environment
      .getNeighborhood(node)
      .getNeighbors
      .iterator()
      .asScala
      .filter(_.getConcentration(targetMolecule) == LocalNode.asInstanceOf[T])
      .toList
  }

  def setSurrogateFor(nodeId: ID): Unit = surrogateForNodes.add(nodeId)

  def setCurrentNeighborhoodOf(nodeId: ID, neighborhood: Set[ID]): Unit =
    currentNeighborhoodOfNodes.put(nodeId, neighborhood)

  def removeSurrogateFor(nodeId: ID): Unit = {
    surrogateForNodes.remove(nodeId)
    contextManager.remove(nodeId)
  }

  def isSurrogateForNode(nodeId: ID): Boolean = surrogateForNodes.contains(nodeId)

  def isSurrogateFor: Set[ID] = surrogateForNodes.toSet

  def setContextFor(nodeId: ID, context: CONTEXT): Unit = contextManager.put(nodeId, context)

  def getComputedResultFor(nodeId: ID): Option[NeighborData[P]] =
    for {
      neighbors <- neighborhoodManager.get(nodeId)
      computedResult <- neighbors.get(nodeId)
    } yield computedResult

  def isComputationalCycleComplete: Boolean = completed

  def prepareForComputationalCycle(): Unit = completed = false
}

object RunSurrogateScafiProgram {
  implicit class RichMap2[K, V](map: Map[K, V]) {
    def mapValuesStrict[T](f: V => T): Map[K, T] = map.map(tp => tp._1 -> f(tp._2))
  }
}
