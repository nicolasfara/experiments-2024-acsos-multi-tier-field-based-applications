package it.unibo.alchemist.exporter

import it.unibo.alchemist.boundary.extractors.AbstractDoubleExporter
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Actionable, Environment, Time}

import java.{lang, util}
import scala.jdk.CollectionConverters.CollectionHasAsScala

class RegionCountExporter extends AbstractDoubleExporter {

  override def getColumnNames: util.List[String] = util.List.of("RegionCount")

  override def extractData[T](
      environment: Environment[T, _],
      actionable: Actionable[T],
      time: Time,
      l: Long
  ): util.Map[String, lang.Double] = {
    val applicativeNodes = environment.getNodes.asScala.filter(_.contains(new SimpleMolecule("WearableDevice")))
    val regionCount = applicativeNodes
      .map(_.getConcentration(new SimpleMolecule("leaderId")).asInstanceOf[Int])
      .toSet
      .size
    util.Map.of("RegionCount", regionCount.toDouble)
  }
}
