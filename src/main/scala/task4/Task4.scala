package task4

import task4.Task4.Expr.{Add, Literal, Multiply, evaluete}

object Task4 extends App:

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

    val expr1: Expr = Add(Literal(2), Literal(3))
    println(Expr.evaluete(expr1))

    val expr2: Expr = Multiply(expr1, expr1)
    println(Expr.show(expr2) + " = " + Expr.evaluete(expr2))

