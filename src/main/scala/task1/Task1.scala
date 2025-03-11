package task1

object Task1 extends App:

  def carriedMult(x: Double)(y: Double): Double = x * y

  val multiBy3 = carriedMult(3)

  assert(multiBy3(3) == 9)

  def concat(s: String, s1: String): String = s + s1

  assert(concat("ci", "ao") == "ciao")

  def curriedConcat(s: String)(s1: String): String = s + s1

  val concatEhi = curriedConcat("Ehi ")

  assert(concatEhi("ciao") == "Ehi ciao")

  val isPositive0: Double => String = (v: Double) => v match
    case v if v >= 0 => "pos"
    case _ => "neg"

  val isPositive1: Double => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def isPositive2(v: Double): String = v match
    case v if v >= 0 => "pos"
    case _ => "neg"

  val empty: String => Boolean = _ == ""

  val neg: (String => Boolean) => String => Boolean = p => s => !p(s)

  val notEmpty = neg(empty)

  def neg2(p: String => Boolean): String => Boolean = s => !p(s)

  val notEmpty2 = neg2(empty)

  assert(notEmpty2("c"))

  def neg[X](p: X => Boolean): X => Boolean = X => !p(X)
