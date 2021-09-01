import cats.data.Validated.{Invalid, Valid}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retcalc.SimulatePlanApp

class SimulatePlanAppIT extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
    "SimulatePlanApp.strMain" should {
        "return an error when the period exceeds the returns bounds" in {
            val actualResult = SimulatePlanApp.strMain(
                Array("1952.09,2017.09", "25", "60", "3000", "2000", "10000")
            )
            val expectedResult = "Cannot get the return for month 780. Accepted range: 0 to 779"
            actualResult should ===(Invalid(expectedResult))
        }

        "return an usage example when the number of arguments is incorrect" in {
            val result = SimulatePlanApp.strMain(
                Array("1952.09:2017.09", "25.0", "60", "3'000", "2000.0")
            )
            result should ===(Invalid(
                """Usage:
                  |simulatePlan from,until nbOfYearsSaving nbOfYearsRetired netIncome currentExpenses initialCapital
                  |
                  |Example:
                  |simulatePlan 1952.09,2017.09 25 40 3000 2000 10000
                  |""".stripMargin
            ))
        }

        "return several errors when several arguments are invalid" in {
            val result = SimulatePlanApp.strMain(
                Array("1952.09:2017.09", "25.0", "60", "3'000", "2000.0", "10000")
            )
            result should ===(Invalid(
                """Invalid format for fromUntil. Expected: from,until, actual: 1952.09:2017.09
                  |Invalid number for nbOfYearsSaving: 25.0
                  |Invalid number for netIncome: 3'000
                  |Invalid number for currentExpenses: 2000.0""".stripMargin
            ))
        }
    }
}
