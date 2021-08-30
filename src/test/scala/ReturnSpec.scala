import org.scalactic.{Equality, TolerantNumerics, TypeCheckedTripleEquals}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import retcalc.{FixedReturns, OffsetReturns, Returns, VariableReturn, VariableReturns}


class ReturnSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
    implicit val doubleEquality: Equality[Double] =
        TolerantNumerics.tolerantDoubleEquality(0.001)

    "VariableReturns.fromUntil" should {
        "keep only a window of the returns" in {
            val variableReturns = VariableReturns(Vector.tabulate(12) { i =>
                val d = (i + 1).toDouble
                VariableReturn(f"2017.$d%02.0f", d)
            })

            variableReturns.fromUntil("2017.07", "2017.09").returns should ===(
                Vector(
                    VariableReturn("2017.07", 7.0),
                    VariableReturn("2017.08", 8.0)
                )
            )

            variableReturns.fromUntil("2017.10", "2018.01").returns should ===(
                Vector(
                    VariableReturn("2017.10", 10.0),
                    VariableReturn("2017.11", 11.0),
                    VariableReturn("2017.12", 12.0)
                )
            )
        }
    }

    "Returns.monthlyRate" should {
        "return a fixed rate for a FixedReturn" in {
            Returns.monthlyRate(FixedReturns(0.04), 0) should ===(0.04 / 12)
            Returns.monthlyRate(FixedReturns(0.04), 10) should ===(0.04 / 12)
        }

        val variableReturns = VariableReturns(Vector(
            VariableReturn("2000.01", 0.1),
            VariableReturn("2000.02", 0.2)
        ))
        "return the nth rate for VariableReturn" in {
            Returns.monthlyRate(variableReturns, 0) should ===(0.1)
            Returns.monthlyRate(variableReturns, 1) should ===(0.2)
        }
        "roll over from the first rate if n > length" in {
            Returns.monthlyRate(variableReturns, 2) should ===(0.1)
            Returns.monthlyRate(variableReturns, 3) should ===(0.2)
            Returns.monthlyRate(variableReturns, 4) should ===(0.1)
        }
    }

    "Returns.monthlyReturn" should {
        "return a fixed rate for a FixedReturn" in pending

        val variableReturns = VariableReturns(
            Vector(VariableReturn("2000.01", 0.1), VariableReturn("2000.02", 0.2))
        )

        "return the nth rate for VariableReturn" in pending

        "return an error if n > length" in pending

        "return the n+offset the rate for OffsetReturn" in {
            val returns = OffsetReturns(variableReturns, 1)
            Returns.monthlyRate(returns, 0) should ===(0.2)
        }
    }
}
