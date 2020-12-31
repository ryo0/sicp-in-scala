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

  val dx = 0.001

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

  def smooth(f: Double => Double): Double => Double = {
    { x =>
      (f(x) + f(x - dx) + f(x + dx)) / 3
    }
  }

  def compose2(
    f: (Double => Double) => Double => Double,
    g: (Double => Double) => Double => Double
  ): (Double => Double) => Double => Double = {
    { x =>
      f(g(x))
    }
  }

  def repeated2(f: (Double => Double) => Double => Double,
                n: Int): (Double => Double) => Double => Double = {
    @tailrec
    def iter(
      i: Int,
      acmF: (Double => Double) => Double => Double
    ): (Double => Double) => Double => Double = {
      if (i == n) {
        acmF
      } else {
        iter(i + 1, compose2(f, acmF))
      }
    }

    iter(1, f)
  }

  repeated2(smooth, 10)

  def aveDamp(f: Double => Double): Double => Double = {
    { x =>
      (x + f(x)) / 2
    }
  }

  def sqrt2(x: Double): Double = {
    fixedPoint(aveDamp({ y =>
      x / y
    }), 1f)
  }

  println(sqrt2(2.0))

  def cubeRoot(x: Double): Double = {
    fixedPoint(aveDamp({ y =>
      x / (y * y)
    }), 1f)
  }

  println(cubeRoot(2.0))

  def quadRoot(x: Double): Double = {
    fixedPoint(repeated2(aveDamp, 2) { y =>
      x / (y * y * y)
    }, 1f)
  }
  println(quadRoot(2.0))

  def nExp(x: Double, n: Int): Double = {
    def iter(i: Int): Double = {
      if (i == n) {
        1
      } else {
        x * iter(i + 1)
      }
    }
    iter(0)
  }
  def nthRoot(x: Double, n: Int, k: Int): Double = {
    fixedPoint(repeated2(aveDamp, k) { y =>
      x / nExp(y, n - 1)
    }, 1f)
  }
  println(nExp(nthRoot(2, 2, 1), 2))
  println(nExp(nthRoot(2, 3, 1), 3))
  println(nExp(nthRoot(2, 4, 2), 4))
  println(nExp(nthRoot(2, 5, 2), 5))
//  https://www.serendip.ws/archives/491
  def iterativeImprove(
    judge: (Double, Double) => Boolean,
    improver: Double => Double
  ): (Double, Double) => Double = {
    def iter(x: Double, y: Double): Double = {
      if (judge(x, y)) { x } else { iter(y, improver(y)) }
    }
    iter
  }
  def fixedPoint2(f: Double => Double, firstGuess: Double): Double = {
    iterativeImprove(areClose, f)(firstGuess, f(firstGuess))
  }
  def newtonMethod2(g: Double => Double, guess: Double): Double = {
    fixedPoint2(newtonTransform(g), guess)
  }

  def sqrt3(x: Double): Double = {
    newtonMethod2({ y =>
      (y * y - x)
    }, 1.0)
  }
  println(sqrt3(2.0))

  def sqrt4(x: Double): Double = {
    def improver(guess: Double, x: Double): Double = {
      (guess + x / guess) / 2
    }
    def judge(guess: Double, x: Double): Boolean = {
      math.abs(guess * guess - x) < 0.001
    }
    def iterativeImprove(
      judge: (Double, Double) => Boolean,
      improver: (Double, Double) => Double
    ): (Double, Double) => Double = {
      @tailrec
      def iter(x: Double, y: Double): Double = {
        if (judge(x, y)) { x } else { iter(improver(x, y), y) }
      }
      iter
    }
    iterativeImprove(judge, improver)(1.0, x)
  }
  println(sqrt4(2.0))
}
