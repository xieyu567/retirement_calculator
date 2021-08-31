import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retcalc.SimulatePlanApp

class SimulatePlanAppIT extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
    "SimulatePlanApp.strMain" should {
        "simulate a retirment plan using market returns" in {
            val actualResult = SimulatePlanApp.strMain(
                Array("1997.09,2017.09", "25", "40", "3000", "2000", "10000")
            )

            val expectedResult =
                s"""
                   |Capital after 25 years of savings: 499923
                   |Capital after 40 years of retirement: 586435
                   |""".stripMargin
            actualResult should ===(expectedResult)
        }
    }
}
