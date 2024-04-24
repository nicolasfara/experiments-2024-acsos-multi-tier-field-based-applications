package it.unibo.alchemist.model.linkingrules

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.neighborhoods.Neighborhoods
import it.unibo.alchemist.model.{Environment, LinkingRule, Neighborhood, Node, Position}

import scala.jdk.CollectionConverters._

import java.util

class ThreeTierConnection[T, P <: Position[P]](
    private val range: Double
) extends LinkingRule[T, P] {

  override def computeNeighborhood(center: Node[T], environment: Environment[T, P]): Neighborhood[T] = {
    val wearableNodes = environment.getNodes.stream().filter(_.contains(new SimpleMolecule("WearableDevice"))).toList.asScala
    val edgeServersNodes = environment.getNodes.stream().filter(_.contains(new SimpleMolecule("EdgeServer"))).toList.asScala
    val cloudNodes = environment.getNodes.stream().filter(_.contains(new SimpleMolecule("CloudInstance"))).toList.asScala

    if (center.contains(new SimpleMolecule("WearableDevice"))) {
      val nearest = environment.getNodesWithinRange(center, range).stream().toList.asScala
      Neighborhoods.make(environment, center, (nearest ++ cloudNodes).asJava)
    } else if (center.contains(new SimpleMolecule("EdgeServer"))) {
      Neighborhoods.make(environment, center, (cloudNodes ++ edgeServersNodes).asJava)
    } else {
        Neighborhoods.make(environment, center, (edgeServersNodes ++ wearableNodes ++ cloudNodes).asJava)
    }
  }

  override def isLocallyConsistent: Boolean = false
}
