package it.unibo.alchemist.model.linkingrules

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.neighborhoods.Neighborhoods
import it.unibo.alchemist.model.{Environment, LinkingRule, LocalNode, Neighborhood, Node, Position, SurrogateNode, Target}

import scala.jdk.CollectionConverters._

class ThreeTierConnection[T, P <: Position[P]](
    private val applicativeRange: Double,
    private val edgeRange: Double
) extends LinkingRule[T, P] {
  private val targetMolecule = new SimpleMolecule("Target")

  override def computeNeighborhood(center: Node[T], environment: Environment[T, P]): Neighborhood[T] = {
    val nodes = environment.getNodes.stream().iterator().asScala.toSet
    if (center.getConcentration(targetMolecule) == SurrogateNode("CloudInstance").asInstanceOf[T]) {
      Neighborhoods.make(environment, center, (nodes - center).asJava)
    } else if (center.getConcentration(targetMolecule) == SurrogateNode("EdgeServer").asInstanceOf[T]) {
      val edgeServersNodes = getNodesWithTarget(nodes, SurrogateNode("EdgeServer"))
      val cloudNodes = getNodesWithTarget(nodes, SurrogateNode("CloudInstance"))
      Neighborhoods.make(environment, center, (edgeServersNodes ++ cloudNodes - center).asJava)
    } else {
      val nearbyApplicativeNodes = environment.getNodesWithinRange(center, applicativeRange).iterator().asScala.toSet
      val nearbyEdgeServers = environment.getNodesWithinRange(center, edgeRange).iterator().asScala.toSet
      val applicativeDevices = getNodesWithTarget(nearbyApplicativeNodes, LocalNode)
      val edgeServersNodes = getNodesWithTarget(nearbyEdgeServers, SurrogateNode("EdgeServer"))
      val cloudNodes = getNodesWithTarget(nodes, SurrogateNode("CloudInstance"))
      Neighborhoods.make(environment, center, (applicativeDevices ++ edgeServersNodes ++ cloudNodes - center).asJava)
    }
  }

  private def getNodesWithTarget(nodes: Set[Node[T]], target: Target): Set[Node[T]] = {
    nodes.filter(_.getConcentration(targetMolecule) == target.asInstanceOf[T])
  }

  override def isLocallyConsistent: Boolean = true
}
