import scala.annotation.tailrec
import scala.math.sqrt

object s1_3_1 {
  println("hello, world")
  def sum(term: Float => Float,
          a: Float,
          next: Float => Float,
          b: Float): Float = {
    if (a > b) {
      0
    } else {
      term(a) + sum(term, next(a), next, b)
    }
  }

  println(sum({ x: Float =>
    x * x * x
  }, 0, { x: Float =>
    x + 1
  }, 10))

  def intergral(f: Float => Float, a: Float, b: Float, dx: Float): Float = {
    def addDx(x: Float): Float = {
      x + dx
    }
    sum(f, a + dx / 2f, addDx, b) * dx
  }
  def cube(x: Float): Float = {
    x * x * x
  }
  println(intergral(cube, 0, 1, 0.001f))

  def co(n: Int): Int = {
    if (n % 2 == 0) {
      2
    } else {
      4
    }
  }
  def simp(a: Float, b: Float, n: Int, f: Float => Float): Float = {
    def h(): Float = {
      (b - a) / n
    }
    def inc(n: Float): Float = {
      n + 1
    }
    def yk(k: Float): Float = {
      co(k.asInstanceOf[Int]) * f(a + k * h)
    }
    (h() / 3) * (yk(0) + yk(n) + sumIter(yk, 1, inc, n - 1))
  }
  println(simp(0, 1, 10000, cube))

  def sumIter(term: Float => Float,
              a: Float,
              next: Float => Float,
              b: Float): Float = {
    @tailrec
    def iter(a: Float, acm: Float): Float = {
      if (a > b) {
        acm
      } else {
        iter(next(a), term(a) + acm)
      }
    }
    iter(a, 0)
  }

  def product(term: Float => Float,
              a: Float,
              next: Float => Float,
              b: Float): Float = {
    if (a > b) {
      1
    } else {
      term(a) * product(term, next(a), next, b)
    }
  }
  def productIter(term: Float => Float,
                  a: Float,
                  next: Float => Float,
                  b: Float): Float = {
    @tailrec
    def iter(a: Float, acm: Float): Float = {
      if (a > b) {
        acm
      } else {
        iter(next(a), term(a) * acm)
      }
    }
    iter(a, 1)
  }
  println(product({ x =>
    x
  }, 1, { x =>
    x + 1
  }, 10))
  def pi(n: Float): Float = {
    // 分子:
    // 0, 1 -> 2
    // 2, 3 -> 4
    // 4, 5 -> 6
    // 6, 7 -> 8
    // …
    // 偶数なら2, 奇数なら1を足せば良い
    // 分母
    // 1, 2 -> 3
    // 3, 4 -> 5
    // 5, 6 -> 7
    // 奇数なら2, 偶数なら1を足せば良い
    def molecule(n: Float): Float = {
      if (n.asInstanceOf[Int] % 2 == 0) {
        n + 2
      } else {
        n + 1
      }
    }
    def denominator(n: Float): Float = {
      if (n.asInstanceOf[Int] % 2 == 0) {
        n + 1
      } else {
        n + 2
      }
    }
    def pik(k: Float): Float = {
      molecule(k) / denominator(k)
    }
    productIter(pik, 1, { x =>
      x + 1
    }, n) * 4
  }
  println(pi(100000))

  def accumulate(combiner: (Float, Float) => Float,
                 nullVal: Float,
                 term: Float => Float,
                 a: Float,
                 next: Float => Float,
                 b: Float): Float = {
    if (a > b) {
      nullVal
    } else {
      combiner(term(a), accumulate(combiner, nullVal, term, next(a), next, b))
    }
  }

  def accumulateIter(combiner: (Float, Float) => Float,
                     nullVal: Float,
                     term: Float => Float,
                     a: Float,
                     next: Float => Float,
                     b: Float): Float = {
    @tailrec
    def iter(a: Float, acm: Float): Float = {
      if (a > b) {
        acm
      } else {
        iter(next(a), combiner(term(a), acm))
      }
    }
    iter(a, nullVal)
  }

  def sum2(term: Float => Float,
           a: Float,
           next: Float => Float,
           b: Float): Float = {
    accumulate({ (x, y) =>
      x + y
    }, 0, term, a, next, b)
  }

  def sum2Iter(term: Float => Float,
               a: Float,
               next: Float => Float,
               b: Float): Float = {
    accumulateIter({ (x, y) =>
      x + y
    }, 0, term, a, next, b)
  }

  def simp2(a: Float, b: Float, n: Int, f: Float => Float): Float = {
    def h(): Float = {
      (b - a) / n
    }
    def inc(n: Float): Float = {
      n + 1
    }
    def yk(k: Float): Float = {
      co(k.asInstanceOf[Int]) * f(a + k * h)
    }
    (h() / 3) * (yk(0) + yk(n) + sum2Iter(yk, 1, inc, n - 1))
  }
  println(simp2(0, 1, 10000, cube))

  def filAccumulate(filter: Int => Boolean,
                    combiner: (Float, Float) => Float,
                    nullVal: Float,
                    term: Float => Float,
                    a: Float,
                    next: Float => Float,
                    b: Float): Float = {
    if (a > b) {
      nullVal
    } else {
      println(filter(a.asInstanceOf[Int]))
      println(a)
      if (filter(a.asInstanceOf[Int])) {
        combiner(
          term(a),
          filAccumulate(filter, combiner, nullVal, term, next(a), next, b)
        )
      } else {
        filAccumulate(filter, combiner, nullVal, term, next(a), next, b)
      }
    }
  }

  def filAccumulateIter(filter: Int => Boolean,
                        combiner: (Float, Float) => Float,
                        nullVal: Float,
                        term: Float => Float,
                        a: Float,
                        next: Float => Float,
                        b: Float): Float = {
    @tailrec
    def iter(a: Float, acm: Float): Float = {
      if (a > b) {
        acm
      } else {
        if (filter(a.asInstanceOf[Int])) {
          iter(next(a), combiner(term(a), acm))
        } else {
          iter(next(a), acm)
        }
      }
    }
    iter(a, nullVal)
  }

  def isPrime(num: Int): Boolean = {
    if (num == 2) {
      true
    } else if (num == 1 || num % 2 == 0) {
      false
    } else {
      (3L to sqrt(num).toLong).filter(n => n % 2 != 0).foreach { e =>
        if (num % e == 0) return false
      }
      true
    }
  }
  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  println(filAccumulateIter(isPrime, { (x, y) =>
    x + y
  }, 0, { x =>
    x * x
  }, 0, { x =>
    x + 1
  }, 10))

  println(filAccumulateIter({ i =>
    10 > i && gcd(i, 10) == 1
  }, { (x, y) =>
    x * y
  }, 1, { x =>
    x
  }, 1, { x =>
    x + 1
  }, 10))
}
