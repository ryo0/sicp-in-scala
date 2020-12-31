import scala.annotation.tailrec

object Main extends App {
  def areClose(a: Double, b: Double): Boolean = {
    math.abs(a - b) < 0.001
  }
  def fixedPoint(f: Double => Double, firstGuess: Double): Double = {
    var counter = 0
    @tailrec
    def tryGuess(guess: Double): Double = {
      val next = f(guess)
      counter += 1
      if (areClose(guess, next)) {
        next
      } else {
        tryGuess(next)
      }
    }
    tryGuess(firstGuess)
  }
  val dx = 0.00001
  def deriv(g: Double => Double): Double => Double = {
    import s1_3_2.areClose
    { x =>
      (g(x + dx) - g(x)) / dx
    }
  }
  def newtonTransform(g: Double => Double): Double => Double = {
    { x =>
      x - g(x) / deriv(g)(x)
    }
  }
  def newtonMethod(g: Double => Double, guess: Double): Double = {
    fixedPoint(newtonTransform(g), guess)
  }
  def sqrt(x: Double): Double = {
    newtonMethod({ y =>
      (y * y - x)
    }, 1.0)
  }
  println(sqrt(2.0))

  def fixedPointOfTransform(g: Double => Double,
                            transform: (Double => Double) => Double => Double,
                            guess: Double): Double = {
    fixedPoint(transform(g), guess)
  }
//  def cubic(a: Double, b: Double, c: Double): Double => Double = {
//    x => (x ** 3 + a * x ** 2 + b * x + c)
//  }
  def double(f: Double => Double): Double => Double = {
    { x: Double =>
      f(f(x))
    }
  }
  println(double({ x =>
    x + 1
  })(0))
  def compose(f: Double => Double, g: Double => Double): Double => Double = {
    { x =>
      f(g(x))
    }
  }
  println(compose({ x =>
    x * x
  }, { x =>
    x + 1
  })(6))

  def repeated(f: Double => Double, n: Int): Double => Double = {
    @tailrec
    def iter(i: Int, acmF: Double => Double): Double => Double = {
      if (i == n) {
        acmF
      } else {
        iter(i + 1, compose(f, acmF))
      }
    }
    iter(1, f)
  }
  println("result is  " + repeated({ x =>
    x * x
  }, 2)(5))
}
