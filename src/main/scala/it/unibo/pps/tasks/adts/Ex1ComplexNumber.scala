package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?

    case class Comp(re: Double, im: Double)

    type Complex = Comp
    def complex(re: Double, im: Double): Complex = Comp(re, im)
    extension (c: Complex)
      def re(): Double = c.re
      def im(): Double = c.im
      def sum(other: Complex): Complex = complex(c.re() + other.re(), c.im() + other.im())
      def subtract(other: Complex): Complex = c.sum(complex(- other.re(), - other.im()))
      def asString(): String = (re, im) match
        case (r, i) if i == 0 => s"${c.re()}"
        case (r, i) if r == 0 => s"${c.im()}i"
        case (r, i) if i < 0 => s"${c.re()} - ${-c.im()}i"
        case _ => s"${c.re()} + ${c.im()}i"
