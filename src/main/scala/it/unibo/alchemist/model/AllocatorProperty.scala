package it.unibo.alchemist.model

import it.unibo.alchemist.model.molecules.SimpleMolecule

import scala.jdk.CollectionConverters.IterableHasAsScala

class AllocatorProperty[T, P <: Position[P]](
    private val environment: Environment[T, _],
    private val node: Node[T],
    private val startAllocation: Map[String, String]
) extends NodeProperty[T] {
  private val currentAllocation = collection.mutable.Map(startAllocation.toSeq: _*)
  private val currentPhysicalAllocation = collection.mutable.Map[String, Int]()

  def getAllocation: Map[String, String] = currentAllocation.toMap
  def moveComponentTo(component: String, targetHost: String): Unit = currentAllocation(component) = targetHost
  def manageAllocationToSurrogates(): Unit = {
    val neighborhood = environment.getNeighborhood(node).asScala.toList
    currentAllocation.foreach { case (component, targetHost) =>
      val candidateHosts = neighborhood
        .filter(_.contains(new SimpleMolecule(targetHost))) // Mimic the capability
        .map(n => { // Check if the host can execute the component
          n -> ScafiSurrogateIncarnationUtils
            .allSurrogateScafiProgramsFor[T, P](n)
            .find(_.asMolecule == new SimpleMolecule(component))
        })
        .filter(_._2.isDefined)
        .map { case (node, program) => node -> program.get }
        .toMap
      require(candidateHosts.size == 1, s"Expected exactly one host for component $component, found ${candidateHosts.size}")
      val (hostToOffload, program) = candidateHosts.head
      // Remove from previous surrogate
      currentPhysicalAllocation.get(component) match {
        case Some(hostId) if hostId == hostToOffload.getId => () // Already offloaded to the same surrogate as before, no need to change
        case Some(hostId) =>
          val node = environment.getNodeByID(hostId)
          ScafiSurrogateIncarnationUtils
            .allSurrogateScafiProgramsFor[T, P](node)
            .find(_.asMolecule == new SimpleMolecule(component)) match {
            case Some(oldProgram) => oldProgram.removeSurrogateFor(node.getId)
            case None             => throw new IllegalStateException(s"Component $component is not offloaded to node $hostId")
          }
        case _ => currentPhysicalAllocation.put(component, hostToOffload.getId)
      }
      println(s"Node ${node.getId} offloads component $component to node ${hostToOffload.getId}")
      program.setSurrogateFor(node.getId)
    }
  }
  override def getNode: Node[T] = node
  override def cloneOnNewNode(node: Node[T]): NodeProperty[T] = ???
}
