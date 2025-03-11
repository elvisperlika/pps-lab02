package task3

object Task3 extends App:

  // NON-TAIL
  def power(base: Double, exp: Int): Double = exp match
    case 0 => 1
    case _ => base * power(base, exp - 1)

  // TAIL
  def powerTail(base: Double, exp: Int): Double =
    @annotation.tailrec
    def _pow(acc: Double, exp: Int): Double = exp match
      case 0 | 1 => acc
      case _ => _pow(base * acc, exp - 1)
    _pow(base, exp)

  // TAIL
  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _rev(n: Int, acc: Int): Int = n match
      case 0 => acc
      case _ => _rev(n / 10, acc * 10 + n % 10)
    _rev(n, 0)
