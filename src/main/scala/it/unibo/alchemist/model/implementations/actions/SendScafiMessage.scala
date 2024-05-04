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
    program.prepareForComputationalCycle()
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

  /** Effectively executes this action. */
//  override def execute(): Unit = {
//    program.getExport(device.getNode.getId) match {
//      case Some(toSend) =>
//        send(toSend)
//        // I need to send the field output to the input program of this one following the programDag
//        for {
//          programsToFeed <- program.programDag.get(program.asMolecule.getName)
//          (pat, valueOption) = program.generateComponentOutputField()
//          value <- valueOption
//        } {
//          programsToFeed.foreach(programName => sendToInputComponent(pat -> value, programName))
//          getLocalComponentsOf(programsToFeed).foreach(_.feedInputFromNode(device.getNode.getId, pat -> value))
//        }
//      case _ => println(s"No data available to send for ${device.getNode.getId}")
//    }
//    getNode.setConcentration(new SimpleMolecule("lastSentMessage"), environment.getSimulation.getTime.asInstanceOf[T])
//    program.prepareForComputationalCycle()
//  }
//
//  private def getLocalComponentsOf(component: List[String]): List[RunScafiProgram[T, P]] =
//    ScafiIncarnationUtils
//      .allScafiProgramsFor[T, P](getNode)
//      .filter(p => component.contains(p.asMolecule.getName))
//      .toList
//
//  private def getNeighborhoodForNode(node: Node[T]): List[Node[T]] =
//    environment
//      .getNeighborhood(device.getNode)
//      .getNeighbors
//      .iterator()
//      .asScala
//      .filter(_.contains(new SimpleMolecule("WearableDevice")))
//      .toList
//
//  private def sendToInputComponent(value: (Path, T), componentName: String): Unit = {
//    for {
//      neighborhood <- getNeighborhoodForNode(device.getNode)
//      component <- ScafiIncarnationUtils
//        .allScafiProgramsFor[T, P](neighborhood)
//      if component.asMolecule == new SimpleMolecule(componentName)
//    } component.feedInputFromNode(device.getNode.getId, value)
//  }
//
//  private def send(toSend: NeighborData[P]): Unit = {
//    for {
//      neighborhood <- getNeighborhoodForNode(device.getNode)
//      action <- ScafiIncarnationUtils
//        .allScafiProgramsFor[T, P](neighborhood)
//        .filter(program.getClass.isInstance(_))
//      if action.programNameMolecule == program.programNameMolecule
//    } action.sendExport(device.getNode.getId, toSend)
//  }

  /** @return The context for this action. */
  override def getContext: Context = Context.NEIGHBORHOOD
}
