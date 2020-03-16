package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    /**
     * Δ = b² - 4ac
     */
    Signal {
      Math.pow(b(), 2) - 4 * (a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    /**
     * If the discriminant is positive, then there are two distinct roots
     * (-b ± √Δ) / 2a
     *
     * If the discriminant is zero, then there is exactly one real root
     * -b / 2a
     *
     * If the discriminant is negative, then there are no real roots
     */
    Signal {
      if (delta() < 0) Set[Double]()
      else {
        val r1 = (-b() + Math.sqrt(delta())) / (2 * a())
        val r2 = (-b() - Math.sqrt(delta())) / (2 * a())
        Set(r1, r2)
      }
    }
  }
}
