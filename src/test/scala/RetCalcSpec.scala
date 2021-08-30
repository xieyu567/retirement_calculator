import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retcalc.{FixedReturns, RetCalc, RetCalcParams, VariableReturn, VariableReturns}

class RetCalcSpec extends AnyWordSpec
    with Matchers with TypeCheckedTripleEquals {
    implicit val doubleEquality: Equality[Double] =
        TolerantNumerics.tolerantDoubleEquality(0.001)

    "RetCalc.futureCapital" should {
        "calculate the amount of savings I will have in n months" in {
            val actual = RetCalc.futureCapital(FixedReturns(0.04), nbOfMonths = 25 * 12,
                netIncome = 3000, currentExpenses = 2000, initialCapital = 10000)
            val expected = 541267.1990
            actual should ===(expected)
        }

        "calculate how much savings will be left after having taken a pension for n months" in {
            val actual = RetCalc.futureCapital(FixedReturns(0.04), nbOfMonths = 40 * 12,
                netIncome = 0, currentExpenses = 2000, initialCapital = 541267.198962)
            val expected = 309867.5316
            actual should ===(expected)
        }
    }

    val params = RetCalcParams(
        nbOfMonthsInRetirement = 40 * 12, netIncome = 3000,
        currentExpenses = 2000, initialCapital = 10000
    )

    "RetCalc.simulatePlan" should {
        "calculate the capital at retirement and the capital after death" in {
            val (capitalAtRetirement, capitalAfterDeath) = {
                RetCalc.simulatePlan(
                    returns = FixedReturns(0.04), params, nbOfMonthsSavings = 25 * 12
                )
            }
            capitalAtRetirement should ===(541267.1990)
            capitalAfterDeath should ===(309867.5316)
        }

        "use different returns for capitalisation and drawdown" in {
            val nbOfMonthsSavings = 25 * 12
            val returns = VariableReturns(
                Vector.tabulate(nbOfMonthsSavings + params.nbOfMonthsInRetirement)(i =>
                    if (i < nbOfMonthsSavings)
                        VariableReturn(i.toString, 0.04 / 12)
                    else
                        VariableReturn(i.toString, 0.03 / 12))
            )
            val (capitalAtRetirement, capitalAfterDeath) =
                RetCalc.simulatePlan(returns, params, nbOfMonthsSavings)
            capitalAtRetirement should ===(541267.1990)
            capitalAfterDeath should ===(-57737.7227)
        }
    }
}
