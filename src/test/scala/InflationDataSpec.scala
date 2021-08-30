import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retcalc.InflationData

class InflationDataSpec extends AnyWordSpec with Matchers {
  "InflationData.fromResource" should {
    "load cpi data from tsv file" in {
      val data = InflationData.fromResource("cpi.tsv").head
      data should ===(InflationData("1900.01", 7.897))
    }
  }
}
