package retcalc

import scala.io.Source

case class InflationData(monthId: String, value: Double)

object InflationData {
  def fromResource(resource: String) =
    Source.fromResource(resource).getLines().drop(1).map { line =>
      val field = line.split("\t")
      InflationData(
        monthId = field(0),
        value = field(1).toDouble
      )
    }.toVector
}
