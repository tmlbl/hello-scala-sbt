object Hello {
  def main(args: Array[String]) = {
    greet("boys and girls")
    val samples = Array(49, 36, 99, 74, 66)
    for (x <- samples) {
      val sq = sqrt(x)
      println(s"The approximate square root of $x is $sq")
    }
    val a = 45
    val b = 78
    val c = gcd(a, b)
    println("The greatest common divisor of "
    + s"$a and $b is $c")
  }
  // Prints a greeting
  def greet(msg: String) = {
    println("Hello " + msg + "!")
  }
  // Gives absolute value
  def abs(x: Double): Double = {
    if (x >= 0) x else -x
  }

  // Newton's square root algorithm
  def sqrt(x: Double): Double = {
    // Determines if the guess is good enough
    def isGoodEnough(guess: Double): Boolean = {
      abs(guess * guess - x) < 0.00001
    }
    // Makes a better guess
    def improve(guess: Double): Double = {
      (guess + x / guess) / 2
    }
    // Iterates until the guess is good enough
    def sqrIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrIter(improve(guess))
    }
    sqrIter(1.0)
  }

  // Euclid's greatest common divisor algorithm
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
}
