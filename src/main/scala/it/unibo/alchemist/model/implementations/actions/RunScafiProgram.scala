/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors listed in the main project's alchemist/build.gradle file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */
package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.actions.AbstractLocalAction
import it.unibo.alchemist.model.{Node, Position, Reaction}
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Time => AlchemistTime, _}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ContextImpl, _}
import it.unibo.alchemist.scala.PimpMyAlchemist._
import it.unibo.scafi.space.Point3D
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath
import org.kaikikm.threadresloader.ResourceLoader

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Try}

sealed class RunScafiProgram[T, P <: Position[P]](
    environment: Environment[T, P],
    node: Node[T],
    reaction: Reaction[T],
    randomGenerator: RandomGenerator,
    programName: String,
    retentionTime: Double,
    programDagMapping: Map[String, List[String]] = Map.empty
) extends AbstractLocalAction[T](node) {

  def this(
      environment: Environment[T, P],
      node: Node[T],
      reaction: Reaction[T],
      randomGenerator: RandomGenerator,
      programName: String
  ) = this(environment, node, reaction, randomGenerator, programName, FastMath.nextUp(1.0 / reaction.getTimeDistribution.getRate))

  declareDependencyTo(Dependency.EVERY_MOLECULE)

  import RunScafiProgram.NeighborData
  val program =
    ResourceLoader.classForName(programName).getDeclaredConstructor().newInstance().asInstanceOf[CONTEXT => EXPORT]
  val programDag = programDagMapping
  val programNameMolecule = new SimpleMolecule(programName)
  lazy val nodeManager = new SimpleNodeManager(node)
  private var neighborhoodManager: Map[ID, NeighborData[P]] = Map()
  private val commonNames = new ScafiIncarnationForAlchemist.StandardSensorNames {}
  private var completed = false
  lazy val allocatorProperty: Option[AllocatorProperty[T, P]] = node.getProperties.asScala
    .find(_.isInstanceOf[AllocatorProperty[T, P]])
    .map(_.asInstanceOf[AllocatorProperty[T, P]])

  private val inputFromComponents = collection.mutable.Map[ID, mutable.Buffer[(Path, T)]]()

  def asMolecule = programNameMolecule

  override def cloneAction(node: Node[T], reaction: Reaction[T]) =
    new RunScafiProgram(environment, node, reaction, randomGenerator, programName, retentionTime)

  implicit def euclideanToPoint(point: P): Point3D = point.getDimensions match {
    case 1 => Point3D(point.getCoordinate(0), 0, 0)
    case 2 => Point3D(point.getCoordinate(0), point.getCoordinate(1), 0)
    case 3 => Point3D(point.getCoordinate(0), point.getCoordinate(1), point.getCoordinate(2))
  }

  private def isOffloadedToSurrogate: Boolean = {
    val res = for {
      allocator <- allocatorProperty
      targetHostKind <- allocator.getAllocation.get(asMolecule.getName)
    } yield targetHostKind == LocalNode
    res match {
      case Some(condition) => condition
      case _               => false
    }
  }

  override def execute(): Unit = {
    import scala.jdk.CollectionConverters._
    val position: P = environment.getPosition(node)
    // NB: We assume it.unibo.alchemist.model.Time = DoubleTime
    //     and that its "time unit" is seconds, and then we get NANOSECONDS
    val alchemistCurrentTime = Try(environment.getSimulation)
      .map(_.getTime)
      .orElse(Failure(new IllegalStateException("The simulation is uninitialized (did you serialize the environment?)")))
      .get
    val currentTime: Long = alchemistTimeToNanos(alchemistCurrentTime)
    if (!neighborhoodManager.contains(node.getId)) {
      neighborhoodManager += node.getId -> NeighborData(factory.emptyExport(), position, Double.NaN)
    }
    neighborhoodManager = neighborhoodManager.filter { case (id, data) =>
      id == node.getId || data.executionTime >= alchemistCurrentTime - retentionTime
    }
    // Delete the input from components that are not in the neighborhood
    neighborhoodManager.keys.foreach(nodeId => {
      if (!inputFromComponents.contains(nodeId)) {
        inputFromComponents -= nodeId
      }
    })
    val deltaTime: Long =
      currentTime - neighborhoodManager.get(node.getId).map(d => alchemistTimeToNanos(d.executionTime)).getOrElse(0L)
    val localSensors = node.getContents.asScala.map { case (k, v) => k.getName -> v }

    val neighborhoodSensors = scala.collection.mutable.Map[CNAME, Map[ID, Any]]()
    val exports: Iterable[(ID, EXPORT)] = neighborhoodManager.view.mapValues(_.exportData)
//    val exports = _exports.map { case (id, exp) =>
//      inputFromComponents.get(id) match {
//        case Some(inputs) => inputs.foreach(input => exp.put(input._1, input._2))
//        case None         =>
//      }
//      id -> exp
//    }

    val context = buildContext(exports, localSensors.toMap, neighborhoodSensors, alchemistCurrentTime, deltaTime, currentTime, position)

    // Check if the program is offloaded to a surrogate
    for {
      allocator <- allocatorProperty
      _ = allocator.manageAllocationToSurrogates()
      targetHostKind <- allocator.getAllocation.get(asMolecule.getName)
      if targetHostKind != LocalNode
      surrogateNodeId <- allocator.getPhysicalAllocation.get(asMolecule.getName) // Where is physical executed this program? (Node ID)
      surrogateNode = environment.getNodeByID(surrogateNodeId)
      surrogateProgram <- ScafiSurrogateIncarnationUtils
        .allSurrogateScafiProgramsFor[T, P](surrogateNode)
        .find(_.asMolecule == asMolecule)
    } {
//      println(s"Node ${node.getId} has forward to $targetHostKind")
      surrogateProgram.setContextFor(node.getId, context)
      completed = true
      return
    }
    // End check ---------------------------------------------------

    // Execute normal program since is executed locally
    val computed = program(context)
    node.setConcentration(programName, computed.root[T]())
    val toSend = NeighborData(computed, position, alchemistCurrentTime)
    neighborhoodManager = neighborhoodManager + (node.getId -> toSend)
    completed = true
  }

  private def alchemistTimeToNanos(time: AlchemistTime): Long = (time.toDouble * 1_000_000_000).toLong

  private def buildContext(
      exports: Iterable[(ID, EXPORT)],
      localSensors: Map[String, T],
      neighborhoodSensors: scala.collection.mutable.Map[CNAME, Map[ID, Any]],
      alchemistCurrentTime: AlchemistTime,
      deltaTime: Long,
      currentTime: Long,
      position: P
  ): CONTEXT = new ContextImpl(node.getId, exports, localSensors, Map.empty) {
    override def nbrSense[T](nsns: CNAME)(nbr: ID): Option[T] =
      neighborhoodSensors
        .getOrElseUpdate(
          nsns,
          nsns match {
            case commonNames.NBR_LAG =>
              neighborhoodManager.mapValuesStrict[FiniteDuration](nbr =>
                FiniteDuration(alchemistTimeToNanos(alchemistCurrentTime - nbr.executionTime), TimeUnit.NANOSECONDS)
              )
            /*
             * nbrDelay is estimated: it should be nbr(deltaTime), here we suppose the round frequency
             * is negligibly different between devices.
             */
            case commonNames.NBR_DELAY =>
              neighborhoodManager.mapValuesStrict[FiniteDuration](nbr =>
                FiniteDuration(
                  alchemistTimeToNanos(nbr.executionTime) + deltaTime - currentTime,
                  TimeUnit.NANOSECONDS
                )
              )
            case commonNames.NBR_RANGE => neighborhoodManager.mapValuesStrict[Double](_.position.distanceTo(position))
            case commonNames.NBR_VECTOR =>
              neighborhoodManager.mapValuesStrict[Point3D](_.position.minus(position.getCoordinates))
            case NBR_ALCHEMIST_LAG =>
              neighborhoodManager.mapValuesStrict[Double](alchemistCurrentTime - _.executionTime)
            case NBR_ALCHEMIST_DELAY =>
              neighborhoodManager.mapValuesStrict(nbr => alchemistTimeToNanos(nbr.executionTime) + deltaTime - currentTime)
          }
        )
        .get(nbr)
        .map(_.asInstanceOf[T])

    override def sense[T](lsns: String): Option[T] = (lsns match {
      case LSNS_ALCHEMIST_COORDINATES  => Some(position.getCoordinates)
      case commonNames.LSNS_DELTA_TIME => Some(FiniteDuration(deltaTime, TimeUnit.NANOSECONDS))
      case commonNames.LSNS_POSITION =>
        val k = position.getDimensions()
        Some(
          Point3D(
            position.getCoordinate(0),
            if (k >= 2) position.getCoordinate(1) else 0,
            if (k >= 3) position.getCoordinate(2) else 0
          )
        )
      case commonNames.LSNS_TIMESTAMP  => Some(currentTime)
      case commonNames.LSNS_TIME       => Some(java.time.Instant.ofEpochMilli((alchemistCurrentTime * 1000).toLong))
      case LSNS_ALCHEMIST_NODE_MANAGER => Some(nodeManager)
      case LSNS_ALCHEMIST_DELTA_TIME =>
        Some(
          alchemistCurrentTime.minus(
            neighborhoodManager.get(node.getId).map(_.executionTime).getOrElse(AlchemistTime.INFINITY)
          )
        )
      case LSNS_ALCHEMIST_ENVIRONMENT => Some(environment)
      case LSNS_ALCHEMIST_RANDOM      => Some(randomGenerator)
      case LSNS_ALCHEMIST_TIMESTAMP   => Some(alchemistCurrentTime)
      case _                          => localSensors.get(lsns)
    }).map(_.asInstanceOf[T])
  }

  def sendExport(id: ID, exportData: NeighborData[P]): Unit = {
    neighborhoodManager += id -> exportData
  }

  def getExport(id: ID): Option[NeighborData[P]] = neighborhoodManager.get(id)

  def isComputationalCycleComplete: Boolean = completed

  def prepareForComputationalCycle(): Unit = completed = false

  def setResultWhenOffloaded(result: T): Unit = node.setConcentration(asMolecule, result)

  def feedInputFromNode(node: ID, value: (Path, T)): Unit = {
    inputFromComponents.get(node) match {
      case Some(inputs) =>
        val newInputs = inputs.filter(!_._1.matches(value._1))
        inputFromComponents += node -> (newInputs += value)
      case None         => inputFromComponents += node -> mutable.Buffer(value)
    }
  }

  def generateComponentOutputField(): (Path, Option[T]) = {
    val path = factory.path(Scope(programName))
    val result = neighborhoodManager(node.getId).exportData.get[T](factory.emptyPath())
    path -> result
  }
}

object RunScafiProgram {
  case class NeighborData[P <: Position[P]](exportData: EXPORT, position: P, executionTime: AlchemistTime)

  implicit class RichMap[K, V](map: Map[K, V]) {
    def mapValuesStrict[T](f: V => T): Map[K, T] = map.map(tp => tp._1 -> f(tp._2))
  }
}
