import scala.annotation.tailrec

object s1_3_2 {
  @tailrec
  def search(f: Float => Float, neg: Float, pos: Float): Float = {
    val mid: Float = (neg + pos) / 2f
    if (areClose(neg, pos)) {
      mid
    } else {
      val testValue = f(mid)
      if (testValue > 0) {
        search(f, neg, mid)
      } else if (testValue < 0) {
        search(f, mid, pos)
      } else {
        mid
      }
    }
  }
  def areClose(a: Float, b: Float): Boolean = {
    math.abs(a - b) < 0.001
  }
  def halfInterval(f: Float => Float, a: Float, b: Float): Float = {
    val aVal = f(a)
    val bVal = f(b)
    if (aVal < 0 && bVal > 0) {
      search(f, a, b)
    } else if (aVal > 0 && bVal < 0) {
      search(f, b, a)
    } else {
      throw new Error("error")
    }
  }
  def sin(x: Float): Float = {
    math.sin(x.asInstanceOf[Double]).asInstanceOf[Float]
  }
  println(halfInterval(sin, 2f, 4f))

  def fixedPoint(f: Float => Float, firstGuess: Float): Float = {
    var counter = 0
    @tailrec
    def tryGuess(guess: Float): Float = {
      val next = f(guess)
      println("guess: " + guess)
      println("next: " + next)
      counter += 1
      if (areClose(guess, next)) {
        println("counter: " + counter)
        next
      } else {
        tryGuess(next)
      }
    }
    tryGuess(firstGuess)
  }
  def sqrt(x: Float): Float = {
    fixedPoint({ y =>
      (y + x / y) / 2f
    }, 1f)
  }
  println(sqrt(2f))
  def gold(): Float = {
    fixedPoint({ x =>
      1 + 1 / x
    }, 1f)
  }
  println(gold())
  def q1_36(): Float = {
    fixedPoint({ x =>
      (math.log(1000) / math.log(x)).asInstanceOf[Float]
    }, 2f)
  }
  println(q1_36()) //counter: 23
  def q1_36_2(): Float = {
    fixedPoint({ x =>
      ((x * math.log(x) + math.log(1000)) / (2 * math.log(x)))
        .asInstanceOf[Float]
    }, 2f)
  }
  println(q1_36_2()) //counter: 7
  //  x = log(1000)/log(x)
  //  x = (x * log(x) + log(1000)) / 2 * log(x)
  def countFrac(n: Int => Float, d: Int => Float, k: Int): Float = {
    def iter(i: Int): Float = {
      val nk = n(i)
      val dk = d(i)
      if (i == k) {
        nk / dk
      } else {
        nk / (dk + iter(i + 1))
      }
    }
    iter(0)
  }
  println(countFrac({ _ =>
    1f
  }, { _ =>
    1f
  }, 1000))
  def countFrac2(n: Int => Float, d: Int => Float, k: Int): Float = {
    @tailrec
    def iter(i: Int, acm: Float): Float = {
      val nk = n(i)
      val dk = d(i)
      if (i == 1) {
        nk / (dk + acm)
      } else {
        iter(i - 1, nk / (dk + acm))
      }
    }
    iter(k, 0f)
  }
  println(countFrac2({ _ =>
    1f
  }, { _ =>
    1f
  }, 1000))

  def napierD(n: Int): Float = {
    // 1 -> 1
    // 2 -> 2
    // 3, 4 -> 1
    // 5 -> 4
    // 6, 7 -> 1
    // 8 -> 6
    // 9, 10 -> 1
    // 3で割ったあまりが0 or 1 -> 1
    // 3で割ったあまりが2 -> (商+1)*2
    if (n % 3 == 2) {
      (n / 3 + 1) * 2
    } else {
      1
    }
  }
  for (i <- 1 to 10) {
    println(napierD(i))
  }
  println(countFrac2({ _ =>
    1f
  }, napierD, 1000) + 2)

  def tanX(x: Float, k: Int): Float = {
    def tanN(k: Int): Float = {
      if (k == 1) {
        x
      } else {
        x * x
      }
    }
    def tanD(k: Int): Float = {
      k * 2 - 1
    }
    def iter(i: Int, acm: Float): Float = {
      val nk = tanN(i)
      val dk = tanD(i)
      if (i == 1) {
        nk / (dk - acm)
      } else {
        iter(i - 1, nk / (dk - acm))
      }
    }
    iter(k, 0f)
  }
  println(tanX(1f, 1000))
}
