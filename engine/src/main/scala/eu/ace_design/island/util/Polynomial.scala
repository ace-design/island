package eu.ace_design.island.util


/**
 * Define a way to evaluate polynomials
 */
object Polynomial {

  /**
   * Create a function supporting the evaluation of a given polynomial (a sequence of coefficient, from x**0 to x**n)
   * @param coefficients the ascending sequence of coefficients to apply
   * @return the value of this polynomial function for x, in [0,1] (threshold by construction)
   */
   def apply(coefficients: Seq[Double]): Double => Double = { x =>
    require(x >= 0.0 && x <= 1.0, "x must be in [0,1]")
    val r = (0.0 /: (0 until coefficients.size)) { (acc, i) => acc + math.pow(x,i) * coefficients(i) }
    math.max(0.0, math.min(1.0, r))
  }

}
