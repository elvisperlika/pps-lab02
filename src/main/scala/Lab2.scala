
import task5.Optional.*

object Lab2 extends App:

  // ------------------------------ TASK 1 ------------------------------
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

// ------------------------------ TASK 2 ------------------------------

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  val p2: (x: Int, y: Int, z: Int) => Boolean = (x, y, z) => x <= y && y == z

  def p3(x: Int, y: Int, z: Int) = x <= y && y == z

  def p4(x: Int)(y: Int)(z: Int) = x <= y && y == z

  // double compose
  def compose[A, B, C](f: A => B, g: C => A): C => B = x => f(g(x))

  // triple compose
  def compose3[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = x => compose(f, compose(g, h))(x)

// ------------------------------ TASK 3 ------------------------------

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

// ------------------------------ TASK 4 ------------------------------

    enum Expr:
      case Literal(const: Int)
      case Add(a: Expr, b: Expr)
      case Multiply(a: Expr, b: Expr)


    object Expr:
      def evaluete(e: Expr): Int = e match
        case Literal(const) => const
        case Add(a, b) => evaluete(a) + evaluete(b)
        case Multiply(a, b) => evaluete(a) * evaluete(b)

      def show(e: Expr): String = e match
        case Literal(const) => const.toString
        case Add(a, b) => "(" + show(a) + "+" + show(b) + ")"
        case Multiply(a, b) => show(a) + " * " + show(b)

    val expr1: Expr = Expr.Add(Expr.Literal(2), Expr.Literal(3))
    println(Expr.evaluete(expr1))

    val expr2: Expr = Expr.Multiply(expr1, expr1)
    println(Expr.show(expr2) + " = " + Expr.evaluete(expr2))

// ------------------------------ TASK 5 ------------------------------

  // TESTS (TDD):

  @Test def filterShouldReturnEmptyWhenEmpty(): Unit = {
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.filter(empty, _ > 2)
    assertTrue(Optional.isEmpty(result))
  }

  @Test def filterMustReturnMaybeIfRespectFilteringAndIsNotEmpty(): Unit = {
    val notEmpty = Optional.Maybe(4)
    val result = Optional.filter(notEmpty, _ > 2)
    assertEquals(4, Optional.orElse(result, 0))
  }
  
  // IMPL:

  def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
    case Maybe(value) => Maybe(f(value))
    case _ => Empty()

  def filter[A, B](optional: Optional[A], f: A => Boolean): Optional[A] = optional match
    case Maybe(value) if f(value) => Maybe(value)
    case _ => Empty()