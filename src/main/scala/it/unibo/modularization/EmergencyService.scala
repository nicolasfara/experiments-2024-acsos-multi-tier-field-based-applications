package it.unibo.modularization

import it.unibo.alchemist.model.{AllocatorProperty, LocalNode}

import scala.jdk.CollectionConverters.CollectionHasAsScala

class EmergencyService extends MyAggregateProgram {

  lazy val allocator: AllocatorProperty[_, _] = alchemistEnvironment
    .getNodeByID(mid)
    .getProperties
    .asScala
    .find(_.isInstanceOf[AllocatorProperty[_, _]])
    .map(_.asInstanceOf[AllocatorProperty[_, _]])
    .get

  override def main() = {
    val hopCount = classicGradient(mid() == 80)
    if (mid() == 80) {
      allocator.moveComponentTo(this.getClass.getName, LocalNode)
    }
    writeEnv("physical-allocation", allocator.getPhysicalAllocation)
    writeEnv("potential", hopCount)
  }
}
