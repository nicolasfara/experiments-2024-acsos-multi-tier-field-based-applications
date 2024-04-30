package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.{Node, Position, Reaction}
import it.unibo.alchemist.model.ScafiIncarnationUtils._
import it.unibo.alchemist.model.implementations.nodes.ScafiDevice
import it.unibo.alchemist.model._
import it.unibo.alchemist.model.actions.AbstractAction
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID
import it.unibo.utils.ScalaJavaInterop.EnvironmentOps

import java.util.stream.Collectors
import scala.jdk.CollectionConverters.CollectionHasAsScala

class SendSurrogateScafiMessage[T, P <: Position[P]](
    environment: Environment[T, P],
    device: ScafiDevice[T],
    reaction: Reaction[T],
    val program: RunSurrogateScafiProgram[T, P]
) extends AbstractAction[T](device.getNode) {
  assert(reaction != null, "Reaction cannot be null")
  assert(program != null, "Program cannot be null")

  override def getContext: Context = Context.NEIGHBORHOOD

  override def cloneAction(destinationNode: Node[T], reaction: Reaction[T]): Action[T] =
    runInScafiDeviceContext[T, Action[T]](
      node = destinationNode,
      message = getClass.getSimpleName + " cannot get cloned on a node of type " + destinationNode.getClass.getSimpleName,
      device => {
        val possibleRef = destinationNode.getReactions
          .stream()
          .flatMap(reaction => reaction.getActions.stream())
          .filter(action => action.isInstanceOf[RunSurrogateScafiProgram[_, _]])
          .map(action => action.asInstanceOf[RunSurrogateScafiProgram[T, P]])
          .collect(Collectors.toList[RunSurrogateScafiProgram[T, P]])
        if (possibleRef.size() == 1) {
          return new SendSurrogateScafiMessage(environment, device, reaction, possibleRef.get(0))
        }
        throw new IllegalStateException(
          "There must be one and one only unconfigured " + RunScafiProgram.getClass.getSimpleName
        )
      }
    )

  override def execute(): Unit = {
    /*
     * Rationale: I need to send back the computed result to the original node since the other `send` will propagate
     * the the neighbors not managed by this device.
     * This is the case when the physical neighborhood is not fully offloaded to this device.
     */
    program.isSurrogateFor.foreach(nodeId => {
      for {
        toSend <- program.getComputedResultFor(nodeId)
        localProgram <- getLocalProgramForNode(nodeId)
      } yield localProgram.sendExport(nodeId, toSend)
    })
    program.prepareForComputationalCycle()
  }

  private def getLocalProgramForNode(nodeId: ID): Option[RunScafiProgram[T, P]] = {
    val localPrograms = for {
      node <- environment.getNodesAsScala
      reactions <- node.getReactions.asScala
      action <- reactions.getActions.asScala
    } yield action match {
      case prog: RunScafiProgram[T, P] => if (program.asMolecule == prog.programNameMolecule) prog else null
      case _                           => null
    }
    localPrograms.filter(_ != null).find(action => action.nodeManager.node.getId == nodeId)
  }
}
