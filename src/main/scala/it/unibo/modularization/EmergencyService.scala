package it.unibo.modularization

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{AllocatorProperty, LocalNode, SurrogateNode}

import scala.jdk.CollectionConverters.CollectionHasAsScala

class EmergencyService extends MyAggregateProgram {


  override def main() = {
    if (mid() == 100) println("DIOBOIA")
    val allocator: Option[AllocatorProperty[_, _]] = {
      val node = alchemistEnvironment.getNodeByID(mid())
      if (!node.contains(new SimpleMolecule("WearableDevice"))) None
      else {
        node
          .getProperties
          .asScala
          .find(_.isInstanceOf[AllocatorProperty[_, _]])
          .map(_.asInstanceOf[AllocatorProperty[_, _]])
      }
    }
    val hopCount = classicGradient(mid() == 80)
    allocator.foreach(alloc => {
        writeEnv("allocation", alloc.getAllocation)
        writeEnv("physical-allocation", alloc.getPhysicalAllocation)
    })
    writeEnv("potential", hopCount)
    if (mid() == 100) {
      println(allocator)
      println(alchemistEnvironment.getNodeByID(mid()).getProperties.asScala)
      allocator.foreach(alloc => alloc.moveComponentTo(this.getClass.getName, LocalNode))
    }
    hopCount
  }
}
