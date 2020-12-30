import scala.annotation.tailrec

object Main extends App {
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
}
