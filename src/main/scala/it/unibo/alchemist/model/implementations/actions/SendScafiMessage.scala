/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors
 * listed in the main project's alchemist/build.gradle.kts file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */

package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.{Node, Position, Reaction, ScafiIncarnationUtils}
import it.unibo.alchemist.model.ScafiIncarnationUtils._
import it.unibo.alchemist.model.implementations.nodes.ScafiDevice
import it.unibo.alchemist.model._
import it.unibo.alchemist.model.actions.AbstractAction
import it.unibo.alchemist.model.implementations.actions.RunScafiProgram.NeighborData
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.Path

import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

class SendScafiMessage[T, P <: Position[P]](
    environment: Environment[T, P],
    device: ScafiDevice[T],
    reaction: Reaction[T],
    val program: RunScafiProgram[T, P]
) extends AbstractAction[T](device.getNode) {
  assert(reaction != null, "Reaction cannot be null")
  assert(program != null, "Program cannot be null")

  private val targetMolecule = new SimpleMolecule("Target")

  /** This method allows to clone this action on a new node. It may result useful to support runtime creation of nodes with the same reaction
    * programming, e.g. for morphogenesis.
    *
    * @param destinationNode
    *   The node where to clone this {@link Action}
    * @param reaction
    *   The reaction to which the CURRENT action is assigned
    * @return
    *   the cloned action
    */
  override def cloneAction(destinationNode: Node[T], reaction: Reaction[T]): Action[T] = ???

  override def execute(): Unit = {
    // Send to physical neighbors
    val applicativeNeighbors = getNeighborsWithTarget(LocalNode)
    applicativeNeighbors.foreach(sendToNode)

    // Get programs to input the computed value
    for {
      componentsToInput <- program.programDag.get(program.programNameMolecule.getName)
      component <- componentsToInput
      neighbor <- applicativeNeighbors :+ getNode // Add self node since is another program instance not having the input field
    } sendToInputOfComponent(neighbor, component)
    program.prepareForComputationalCycle()
  }

  private def sendToInputOfComponent(node: Node[T], component: String): Unit = {
    val inputProgram = ScafiIncarnationUtils
      .allScafiProgramsFor[T, P](node)
      .find(_.asMolecule.getName == component)
      .getOrElse(throw new IllegalStateException(s"Program $component not found on node ${node.getId}"))
    val (path, optionalValue) = program.generateComponentOutputField()
    optionalValue match {
      case Some(value) => inputProgram.feedInputFromNode(device.getNode().getId, path -> value)
      case _ => println(s"No data available to feed input of $component on node ${node.getId} from ${device.getNode.getId}")
    }
  }

  private def sendToNode(node: Node[T]): Unit = {
    val programNode = ScafiIncarnationUtils
      .allScafiProgramsFor[T, P](node)
      .find(_.programNameMolecule == program.programNameMolecule)
      .getOrElse(throw new IllegalStateException(s"Program ${program.programNameMolecule} not found on node ${node.getId}"))
    program.getExport(device.getNode().getId) match {
      case Some(toSend) => programNode.sendExport(device.getNode().getId, toSend)
      case _            => println(s"No data available to send for ${device.getNode.getId} to ${node.getId}, maybe the program has been forwarded")
    }
  }

  private def getNeighborsWithTarget(target: Target): List[Node[T]] = {
    environment
      .getNeighborhood(getNode)
      .getNeighbors
      .iterator()
      .asScala
      .filter(_.getConcentration(targetMolecule) == target.asInstanceOf[T])
      .toList
  }

  /** @return The context for this action. */
  override def getContext: Context = Context.NEIGHBORHOOD
}
