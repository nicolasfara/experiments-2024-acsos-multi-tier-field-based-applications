package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.{Action, AllocatorProperty, Environment, LocalNode, Node, Position, PowerModelProperty, Reaction, RoutingService, SurrogateNode}
import it.unibo.alchemist.model.actions.AbstractLocalAction
import it.unibo.alchemist.model.maps.actions.GPSTraceWalker
import it.unibo.alchemist.model.molecules.SimpleMolecule

import scala.jdk.CollectionConverters.CollectionHasAsScala

class ModularizationRuntime[T, P <: Position[P]](
    private val env: Environment[T, P],
    private val node: Node[T],
    private val scenario: String,
) extends AbstractLocalAction[T](node) {
  private lazy val allocator = node.getProperties.asScala
    .filter(_.isInstanceOf[AllocatorProperty[T, P]])
    .map(_.asInstanceOf[AllocatorProperty[T, P]])
    .head
  private lazy val powerModel = node.getProperties.asScala
    .filter(_.isInstanceOf[PowerModelProperty[T, P]])
    .map(_.asInstanceOf[PowerModelProperty[T, P]])
    .head

  override def cloneAction(node: Node[T], reaction: Reaction[T]): Action[T] = ???

  override def execute(): Unit = {
    powerModel.updatePowerModelForNode()
    allocator.checkAllocation()
    if (env.getSimulation.getTime.toDouble > 900.0) {
      stopMoving()
    }
    if (scenario == "offloaded" && powerModel.getBatteryLevel < 1200.0) {
      allocator.setComponentAllocation("it.unibo.modularization.EmergencyComponent", SurrogateNode("EdgeServer"))
    }
    node.setConcentration(new SimpleMolecule("CurrentNodeAllocation"), allocator.getComponentsAllocation.asInstanceOf[T])
    node.setConcentration(
      new SimpleMolecule("CurrentPhysicalAllocation"),
      allocator.getPhysicalComponentsAllocations.map(v => v._1 -> v._2.getId).asInstanceOf[T]
    )
  }

  private def stopMoving(): Unit = {
    node.getReactions.asScala
      .find(react => react.getActions.asScala.exists(_.isInstanceOf[GPSTraceWalker[_, _, _]]))
      .foreach(reaction => {
        node.removeReaction(reaction)
        env.getSimulation.reactionRemoved(reaction)
      })
  }
}
