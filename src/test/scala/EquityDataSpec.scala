import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retcalc.EquityData

class EquityDataSpec extends AnyWordSpec with Matchers {
    "EquityData.fromResource" should {
        "load market data from a tsv file" in {
            val data = EquityData.fromResource("sp500.tsv").head
            data should ===(
                EquityData("1900.01", 6.10, 0.22),
            )
        }

        "EquityData.monthlyDividend" should {
            "return a monthly dividend" in {
                EquityData("2016.09", 2157.69, 45.03).monthlyDividend should ===(45.03 / 12)
            }
        }
    }
}
