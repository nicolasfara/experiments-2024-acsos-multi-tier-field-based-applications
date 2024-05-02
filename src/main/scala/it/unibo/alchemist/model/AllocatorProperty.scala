package it.unibo.alchemist.model

import it.unibo.alchemist.model.implementations.actions.RunSurrogateScafiProgram
import it.unibo.alchemist.model.molecules.SimpleMolecule

import scala.jdk.CollectionConverters.IterableHasAsScala

sealed trait Target
final case class SurrogateNode(kind: String) extends Target
case object LocalNode extends Target

class AllocatorProperty[T, P <: Position[P]](
    private val environment: Environment[T, _],
    private val node: Node[T],
    private val startAllocation: Map[String, Target]
) extends NodeProperty[T] {
  private val currentAllocation = collection.mutable.Map(startAllocation.toSeq: _*)
  private val currentPhysicalAllocation = collection.mutable.Map[String, Int]()

  def getAllocation: Map[String, Target] = currentAllocation.toMap
  def getPhysicalAllocation: Map[String, Int] = currentPhysicalAllocation.toMap
  def moveComponentTo(component: String, toTarget: Target): Unit = {
    toTarget match {
      case LocalNode           => manageLocalAllocation(component)
      case SurrogateNode(kind) => manageSurrogateAllocation(component, SurrogateNode(kind))
    }
    currentAllocation(component) = toTarget
  }

  private def manageLocalAllocation(component: String): Unit = {
//    if (currentAllocation(component) == LocalNode) { // Already allocated to the local node
//      currentPhysicalAllocation.put(component, node.getId)
//      return
//    }

    val neighborhood = environment.getNeighborhood(node).asScala.toList
    val previousAllocation = currentAllocation(component)
//    println(s"Node ${node.getId} is moving $component from $previousAllocation to LocalNode")

    previousAllocation match {
      case LocalNode => // Already allocated to the local node
      case SurrogateNode(_) =>
        val candidates = neighborhood
          .filter(_.contains(new SimpleMolecule(previousAllocation.asInstanceOf[SurrogateNode].kind))) // Mimic the capability
          .map(node => node -> getSurrogateComponentForNode(node, component))
          .filter(_._2.isDefined)
          .map(node => node._1 -> node._2.get)
          .toMap
        require(candidates.size == 1, s"Expected exactly one host for component $component, found ${candidates.size}")
        val (_, oldSurrogateProgram) = candidates.head

        oldSurrogateProgram.removeSurrogateFor(node.getId)
    }
    currentPhysicalAllocation.put(component, node.getId)
  }

  private def manageSurrogateAllocation(component: String, toHost: SurrogateNode): Unit = {
//    if (currentAllocation(component) == toHost) return // Already allocated to the same host

    val neighborhood = environment.getNeighborhood(node).asScala.toList
    val candidates = neighborhood
      .filter(_.contains(new SimpleMolecule(toHost.kind))) // Mimic the capability
      .map(node => node -> getSurrogateComponentForNode(node, component))
      .filter(_._2.isDefined)
      .map { case (node, program) => node -> program.get }
      .toMap
    require(candidates.size == 1, s"Expected exactly one host for component $component, found ${candidates.size}")
    val (hostToOffload, program) = candidates.head
    // Remove from previous surrogate
    val oldSurrogate = getSurrogateComponentForNode(hostToOffload, component)
      .getOrElse(throw new IllegalStateException(s"Component $component is not offloaded to any node"))
    oldSurrogate.removeSurrogateFor(node.getId)
    // Set the new surrogate
    program.setSurrogateFor(node.getId)
    currentPhysicalAllocation.put(component, hostToOffload.getId)
  }

  private def getSurrogateComponentForNode(node: Node[T], program: String): Option[RunSurrogateScafiProgram[T, P]] =
    ScafiSurrogateIncarnationUtils
      .allSurrogateScafiProgramsFor(node)
      .find(_.asMolecule == new SimpleMolecule(program))

  def manageAllocationToSurrogates(): Unit = {
    currentAllocation.foreach { case (component, targetHost) =>
      targetHost match {
        case LocalNode           => manageLocalAllocation(component)
        case SurrogateNode(kind) => manageSurrogateAllocation(component, SurrogateNode(kind))
      }
    }
  }
  override def getNode: Node[T] = node
  override def cloneOnNewNode(node: Node[T]): NodeProperty[T] = ???
}
