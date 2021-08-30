package retcalc

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int,
                         currentExpenses: Int, initialCapital: Double)

object RetCalc {
    def futureCapital(returns: Returns, nbOfMonths: Int,
                      netIncome: Int, currentExpenses: Int,
                      initialCapital: Double): Double = {
        val monthIncome: Int = netIncome - currentExpenses

        (0 until nbOfMonths).foldLeft(initialCapital) {
            case (acc, month) =>
                acc * (1 + Returns.monthlyRate(returns, month)) + monthIncome
        }
    }

    def simulatePlan(returns: Returns, params: RetCalcParams,
                     nbOfMonthsSavings: Int): (Double, Double) = {
        import params._
        val capitalAtRetirement: Double = futureCapital(
            returns = returns, nbOfMonths = nbOfMonthsSavings,
            netIncome = netIncome, currentExpenses = currentExpenses,
            initialCapital = initialCapital)

        val capitalAfterDeath: Double = futureCapital(
            returns = OffsetReturns(returns, nbOfMonthsSavings),
            nbOfMonths = nbOfMonthsInRetirement,
            netIncome = 0, currentExpenses = currentExpenses,
            initialCapital = capitalAtRetirement
        )

        (capitalAtRetirement, capitalAfterDeath)
    }

    def nbOfMonthsSaving(returns: Returns, params: RetCalcParams,
                         nbOfMonthsSavings: Int): Int = {
        import params._
        @tailrec
        def loop(months: Int): Int = {
            val capitalAfterDeath: Double = simulatePlan(returns = returns,
                params: RetCalcParams, nbOfMonthsSavings: Int)._2

            if (capitalAfterDeath > 0.0) months else loop(months + 1)
        }

        if (netIncome > currentExpenses)
            loop(0)
        else
            Int.MaxValue
    }
}
