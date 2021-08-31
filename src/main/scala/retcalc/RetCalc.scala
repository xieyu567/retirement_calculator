package retcalc

import retcalc.RetCalcError.MoreExpensesThanIncome

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int,
                         currentExpenses: Int, initialCapital: Double)

object RetCalc {
    def futureCapital(returns: Returns, nbOfMonths: Int,
                      netIncome: Int, currentExpenses: Int,
                      initialCapital: Double): Either[RetCalcError, Double] = {
        val monthIncome: Int = netIncome - currentExpenses

        (0 until nbOfMonths).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)) {
            case (accumlator, month) =>
                for {
                    acc <- accumlator
                    monthlyRate <- Returns.monthlyRate(returns, month)
                } yield acc * (1 + monthlyRate) + monthIncome
        }
    }

    def simulatePlan(returns: Returns, params: RetCalcParams, nbOfMonthsSavings: Int,
                     monthOffset: Int = 0): Either[RetCalcError, (Double, Double)] = {
        import params._
        for {
            capitalAtRetirement <- futureCapital(
                returns = OffsetReturns(returns, monthOffset), nbOfMonths = nbOfMonthsSavings,
                netIncome = netIncome, currentExpenses = currentExpenses,
                initialCapital = initialCapital)
            capitalAfterDeath <- futureCapital(
                returns = OffsetReturns(returns, monthOffset + nbOfMonthsSavings),
                nbOfMonths = nbOfMonthsInRetirement,
                netIncome = 0, currentExpenses = currentExpenses,
                initialCapital = capitalAtRetirement
            )
        } yield (capitalAtRetirement, capitalAfterDeath)
    }

    def nbOfMonthsSaving(returns: Returns,
                         params: RetCalcParams): Either[RetCalcError, Int] = {
        import params._
        def loop(months: Int): Either[RetCalcError, Int] =
            simulatePlan(returns, params, months).flatMap( i =>
                if (i._2 > 0.0) Right(months)
                else loop(months + 1)
            )


        if (netIncome > currentExpenses)
            loop(0)
        else
            Left(MoreExpensesThanIncome(netIncome, currentExpenses))
    }
}
