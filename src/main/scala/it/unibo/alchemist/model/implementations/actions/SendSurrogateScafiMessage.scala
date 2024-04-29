package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.{Node, Position, Reaction, ScafiIncarnationUtils}
import it.unibo.alchemist.model.ScafiIncarnationUtils._
import it.unibo.alchemist.model.implementations.nodes.ScafiDevice
import it.unibo.alchemist.model._
import it.unibo.alchemist.model.actions.AbstractAction
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID
import it.unibo.utils.ScalaJavaInterop.EnvironmentOps

import java.util.stream.Collectors
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IterableHasAsScala}

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
    program
      .isSurrogateFor()
      .foreach(nodeId => {
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
      case prog: RunScafiProgram[T, P] => if (program.programNameMolecule == prog.programNameMolecule) prog else null
      case _ => null
    }
    localPrograms.filter(_ != null).find(action => action.nodeManager.node.getId == nodeId)
  }

//  /** Get the surrogate programs running on behalf of the neighbors of the device.
//    * @return
//    *   the list of surrogate programs running on behalf of the neighbors of the device.
//    */
//  private def neighborsExecutingOnSurrogatesFor(nodeId: ID): List[RunSurrogateScafiProgram[T, P]] = {
//    val surrogateActions = for {
//      node <- environment.getNodesAsScala
//      reactions <- node.getReactions.asScala
//      action <- reactions.getActions.asScala
//    } yield action match {
//      case prog: RunSurrogateScafiProgram[T, P] =>
//        if (program.programNameMolecule == prog.programNameMolecule) prog else null
//      case _ => null
//    }
//    val neighborsId = environment.getNeighborhood(environment.getNodeByID(nodeId)).asScala.map(_.getId)
//    surrogateActions.filter(_ != null).filter(action => neighborsId.exists(action.isSurrogateForNode))
//  }
//
//  /** Get the neighbors of the device that are executing the program on the local device.
//    * @return
//    *   the list of neighbors of the device that are executing the program on the local device.
//    */
//  private def neighborsExecutingOnLocalDeviceFor(nodeId: ID): List[RunScafiProgram[T, P]] = {
//    val localActions = for {
//      node <- environment.getNodesAsScala
//      reactions <- node.getReactions.asScala
//      action <- reactions.getActions.asScala
//    } yield action match {
//      case prog: RunScafiProgram[T, P] =>
//        if (program.programNameMolecule == prog.programNameMolecule) prog else null
//      case _ => null
//    }
//    val neighborsId = environment.getNeighborhood(environment.getNodeByID(nodeId)).asScala.map(_.getId)
//    localActions.filter(_ != null).filter(action => neighborsId.exists(_ == action.nodeManager.node.getId))
//  }
}
