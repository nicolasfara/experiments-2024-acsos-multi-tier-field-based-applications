package it.unibo.alchemist.model

import it.unibo.alchemist.model.implementations.actions.{RunScafiProgram, RunSurrogateScafiProgram}
import it.unibo.alchemist.model.molecules.SimpleMolecule

import scala.jdk.CollectionConverters.IterableHasAsScala

sealed trait Target
final case class SurrogateNode(kind: String) extends Target
case object LocalNode extends Target

class AllocatorProperty[T, P <: Position[P]](
    environment: Environment[T, _],
    node: Node[T],
    startAllocation: Map[String, Target]
) extends NodeProperty[T] {
  private val previousAllocation = collection.mutable.Map(startAllocation.toSeq: _*)
  private val componentsAllocation = collection.mutable.Map(startAllocation.toSeq: _*)
  private val physicalComponentsAllocations = collection.mutable.Map[String, Int]()

  private val targetMolecule = new SimpleMolecule("Target")

  def getComponentsAllocation: Map[String, Target] = componentsAllocation.toMap
  def getPhysicalComponentsAllocations: Map[String, Node[T]] = physicalComponentsAllocations
    .map(v => v._1 -> environment.getNodeByID(v._2))
    .toMap

  def setComponentAllocation(component: String, target: Target): Unit = {
    componentsAllocation(component) = target
  }

  def checkAllocation(): Unit = {
    val surrogateNeighbors = this.surrogateNeighbors
    if (previousAllocation == componentsAllocation) {
      val neighborhoodIds = surrogateNeighbors.map(_.getId) + node.getId
      // If the neighborhood is the same as the physical allocation, then no changes are needed
      if (physicalComponentsAllocations.isEmpty) {
        componentsAllocation.foreach { case (component, target) =>
          target match {
            case LocalNode                          => physicalComponentsAllocations.put(component, node.getId)
            case surrogateTarget @ SurrogateNode(_) => setSurrogateForComponentWithTarget(component, surrogateTarget)
          }
        }
      } else if (physicalComponentsAllocations.values.forall(neighborhoodIds.contains) && physicalComponentsAllocations.nonEmpty) {
        ()
      } else {
        // If the neighborhood is changed, then the components must be moved accordingly
        // Get changed components
        val changedIds = physicalComponentsAllocations.values.toSet -- neighborhoodIds
        val componentsToRelocate = physicalComponentsAllocations.filter { case (_, id) => changedIds.contains(id) }.toMap
        relocateComponents(componentsToRelocate)
      }
    } else {
      componentsAllocation.foreach { case (component, target) =>
        if (!previousAllocation.get(component).contains(target)) {
          val oldId = physicalComponentsAllocations(component)
          getComponentProgramFromSurrogate(environment.getNodeByID(oldId), component) match {
            case Some(oldComponent) =>
              oldComponent.removeSurrogateFor(node.getId)
              physicalComponentsAllocations.remove(component)
            case None => throw new IllegalStateException(s"Component $component of node ${node.getId} is not offloaded to any node")
          }
          target match {
            case LocalNode                          => physicalComponentsAllocations.put(component, node.getId) // Move to local
            case surrogateTarget @ SurrogateNode(_) => setSurrogateForComponentWithTarget(component, surrogateTarget)
          }
        }
      }
    }
    // replace previous allocation with the current one
    previousAllocation.clear()
    previousAllocation ++= componentsAllocation
  }

  private def setSurrogateForComponentWithTarget(component: String, target: Target): Unit = {
    val result = for {
      newNode <- getNeighborWithTarget(target)
      newComponent <- getComponentProgramFromSurrogate(newNode, component)
    } yield {
      newComponent.setSurrogateFor(node.getId)
      physicalComponentsAllocations(component) = newNode.getId
    }
    if (result.isEmpty) throw new IllegalStateException(s"Cannot move component $component to any node with target $target for node ${node.getId}")
  }

  private def relocateComponents(componentsToRelocate: Map[String, Int]): Unit = {
    componentsToRelocate.foreach { case (component, oldId) =>
      val requiredTargetByComponent = componentsAllocation(component)
      val result = for {
        newNode <- getNeighborWithTarget(requiredTargetByComponent)
        oldComponent <- getComponentProgramFromSurrogate(environment.getNodeByID(oldId), component)
        newComponent <- getComponentProgramFromSurrogate(newNode, component)
      } yield {
        oldComponent.removeSurrogateFor(node.getId)
        newComponent.setSurrogateFor(node.getId)
        physicalComponentsAllocations(component) = newNode.getId
      }
      if (result.isEmpty) throw new IllegalStateException(s"Cannot move component $component to any node")
    }
  }

  private def getComponentProgramFromSurrogate(node: Node[T], component: String): Option[RunSurrogateScafiProgram[T, P]] = {
    ScafiSurrogateIncarnationUtils
      .allSurrogateScafiProgramsFor[T, P](node)
      .find(_.asMolecule.getName == component)
  }

  private def getComponentProgramFromLocal(node: Node[T], component: String): Option[RunScafiProgram[T, P]] = {
    ScafiIncarnationUtils
      .allScafiProgramsFor[T, P](node)
      .find(_.asMolecule.getName == component)
  }

  private def getNeighborWithTarget(target: Target): Option[Node[T]] = {
    environment
      .getNeighborhood(node)
      .asScala
      .toList
      .filter(_.getConcentration(targetMolecule) == target.asInstanceOf[T])
      .sortBy(environment.getDistanceBetweenNodes(node, _))
      .headOption
  }

  private def surrogateNeighbors: Set[Node[T]] = environment
    .getNeighborhood(node)
    .asScala
    .toList
    .filter(_.getConcentration(targetMolecule) != LocalNode.asInstanceOf[T])
    .toSet

  override def getNode: Node[T] = node
  override def cloneOnNewNode(node: Node[T]): NodeProperty[T] = ???
}
