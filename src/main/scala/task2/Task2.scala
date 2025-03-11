package task2

object Task2 extends App:

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  val p2: (x: Int, y: Int, z: Int) => Boolean = (x, y, z) => x <= y && y == z

  def p3(x: Int, y: Int, z: Int) = x <= y && y == z

  def p4(x: Int)(y: Int)(z: Int) = x <= y && y == z

  // double compose
  def compose[A, B, C](f: A => B, g: C => A): C => B = x => f(g(x))

  // triple compose
  def compose3[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = x => compose(f, compose(g, h))(x)