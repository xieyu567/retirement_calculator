package retcalc

import scala.annotation.tailrec

sealed trait Returns

case class FixedReturns(annualRate: Double) extends Returns

case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
    def fromUntil(monthIdFrom: String, monthIdUntil: String): VariableReturns = {
        VariableReturns(
            returns
                .dropWhile(_.monthId != monthIdFrom)
                .takeWhile(_.monthId != monthIdUntil))
    }
}

case class VariableReturn(monthId: String, monthlyRate: Double)

case class OffsetReturns(orig: Returns, offset: Int) extends Returns

object Returns {
    @tailrec
    def monthlyRate(returns: Returns, month: Int): Double = returns match {
        case FixedReturns(r) => r / 12
        case VariableReturns(rs) => rs(month % rs.length).monthlyRate
        case OffsetReturns(rs, offset) => monthlyRate(rs, month + offset)
    }
}
