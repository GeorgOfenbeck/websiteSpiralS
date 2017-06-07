/**
  * Created by rayda on 27-Jan-17.
  */
object ThesisBackground extends App {


  def Twiddles(n: Int, d: Int): VC = ???

  case class Complex(re: Double, im: Double) {
    def +(that: Complex): Complex = ???

    def *(that: Complex): Complex = ???

    def -(that: Complex): Complex = ???

    def /(that: Complex): Complex = ???
  }


  type VC = Array[Complex]

  abstract class SPL {
    def apply(v: VC): VC
  }

  case class I(n: Int) extends SPL {
    def apply(v: VC): VC = v
  }

  case class DFT2() extends SPL {
    def apply(v: VC): VC = Array(v(0) + v(1), v(0) - v(1))
  }

  case class Tensor(f: VC => VC) extends SPL {
    def apply(v: VC): VC = f(v)
  }


  case class L(n: Int, k: Int) extends SPL {
    def apply(v: VC): VC = {
      val out = new VC(v.size)
      for (i <- 0 until n) out(i) = v(i / (n / k) * k * (i % (n / k)))
      out
    }
  }

  case class T(n: Int, d: Int) extends SPL {
    def apply(v: VC): VC = {
      val diag = Twiddles(n, d)
      for (i <- 0 until v.size) v(i) = v(i) * diag(i)
      v
    }
  }


  def tensor(l: SPL, r: SPL): SPL = {
    (l, r) match {
      case (I(n), a) => Tensor((in: VC) => in.grouped(in.size / n).flatMap(chunk => a(chunk)).toArray)
      case (a, I(n)) => Tensor((in: VC) => in.grouped(n).toArray.transpose.map(chunk => a(chunk)).transpose.flatten)
    }
  }


  case class DFT4() extends SPL {
    def apply(v: VC): VC = {
      tensor(DFT2(), I(2))(T(4, 2)(tensor(I(2), DFT2())(L(4, 2)(v))))
    }
  }

  case class DFT_SPL(n: Int) extends SPL {
    def apply(v: VC): VC = {
      if (v.size == 2) DFT2()(v) else {
        val k: Int = ???
        //choose radix
        val m = n / k
        tensor(DFT_SPL(k), I(m))(T(n, m)(tensor(I(k), DFT_SPL(m))(L(n, k)(v))))
      }
    }
  }


  /*abstract class IM {
    def apply(i: Int, v: Int): Int
  }
*/
  abstract class IM {
    def apply(i: Int): Int
  }


  case class IMC(g: IM, f: IM) extends IM {
    def apply(i: Int): Int = g(f(i))
  }

  case class d() extends IM {
    def apply(i: Int): Int = i
  }

  case class h(base: Int, s: Int) extends IM {
    def apply(i: Int): Int = base + i * s
  }

  /*case class h(base: Int, s0: Int, s1: Int) extends IM {
    def apply(i: Int, v: Int): Int = base + (s0 * i) + (s1 * v)
  }*/

  case class l(n: Int, k: Int) extends IM {
    def apply(i: Int): Int = i / (n / k) + k * (i % (n / k))
  }


  case class Perm(im: IM) extends SPL {
    def apply(in: VC): VC = {
      val out = new VC(in.size)
      for (i <- 0 until in.size) out(i) = in(im(i))
      out
    }
  }

  case class diag(im: IM) extends SPL {
    def apply(v: VC): VC = {
      ???
    }
  }

  case class Tdiag(diag: VC, im: IM, loopi: Int) extends SPL {
    def apply(v: VC): VC = {
      for (i <- 0 until v.size) v(i) = v(i) * diag(im(i))
      v
    }
  }


  case class Gather(im: IM, n: Int, N: Int) extends SPL {
    def apply(in: VC): VC = {
      val out = new VC(n)
      for (i <- 0 until n) out(i) = in(im(i))
      out
    }
  }

  //target added for the zero addition
  case class Scatter(target: VC, im: IM, n: Int, N: Int) extends SPL {
    def apply(in: VC): VC = {
      for (i <- 0 until n) target(im(i)) = in(i)
      target
    }
  }

  def sigmasum(end: Int, out: VC, f: (Int, VC, VC) => VC): SPL = SigmaSum(end, out, f)


  case class SigmaSum(end: Int, out: VC, f: (Int, VC, VC) => VC) extends SPL {
    def apply(v: VC): VC = {
      for (i <- 0 to end)
        f(i, v, out)
      out
    }
  }

  /*    def apply(in: VC, f: (Int, VC, VC) => VC): VC = {
        val out = new VC(in.size)
        for (i <- 0 to end)
          f(i, in, out)
        out
      }*/


  //before merge

  case class DFT_SSPL(n: Int) extends SPL {
    def apply(v: VC): VC = {
      if (v.size == 2) DFT2()(v) else {
        val k: Int = ???
        //choose radix
        val m = n / k
        val afterperm = Perm(l(n, k))(v)
        val right: SPL = sigmasum(m - 1, new VC(n), (i: Int, in: VC, out: VC) => {
          Scatter(out, h(i * k, 1), k, n)(DFT_SSPL(k)(Gather(h(i * k, 1), k, in.size)(in)))
        })
        val scale = diag(d())(right(afterperm))
        val left = sigmasum(k - 1, new VC(n), (i: Int, in: VC, out: VC) => {
          Scatter(out, h(i, k), k, n)(DFT_SSPL(k)(Gather(h(i, k), k, in.size)(in)))
        })
        left(scale)
      }
    }
  }

  //after merge
  case class DFT(n: Int) extends SPL {
    def apply(v: VC): VC = {
      if (v.size == 2) DFT2()(v) else {
        val k: Int = ???
        //choose radix
        val m = n / k
        val right: SPL = sigmasum(m - 1, new VC(v.size), (i: Int, in: VC, out: VC) => {
          Scatter(out, h(i * k, 1), k, in.size)(DFT_SSPL(k)(Gather(h(i, m), k, in.size)(in)))
        })
        val left = sigmasum(k - 1, new VC(v.size), (i: Int, in: VC, out: VC) => {
          Scatter(out, h(i, k), k, in.size)(DFT_SSPL(k)(diag(d())(Gather(h(i, k), k, in.size)(right(in)))))
        })
        left(right(v))
      }
    }
  }

  case class GT(end: Int, gh: IM, sh: IM, A: SPL, n: Int) {
    def apply(in: VC): VC = {
      val out = new VC(in.size)
      for (i <- 0 to end) {
        Scatter(out, sh, n, in.size)(A(Gather(sh, n, in.size)(in)))
      }
      out
    }
  }

  /*
    case class DFT_GT(n: Int) extends SPL {
      def apply(v: VC): VC = {
        if (v.size == 2) DFT2()(v) else {
          val k: Int = ???
          //choose radix
          val m = n / k
          val afterperm = Perm(l(n, k))(v)
          val right = GT(m - 1, h(0, k, 1), h(0, k, 1), DFT_GT(k), k)(afterperm)
          val scale = Tdiag(Twiddles(n, m), idim(), 0)(right)
          val left = GT(k - 1, h(0, m, 1), h(0, m, 1), DFT_GT(m), m)(scale)
          left
        }
      }
    }*/


  val result = ???
  /*

  for (i <- 0 until n)
    y(i) = x(f(i))

  for (i <- 0 until n)
    y(f(i)) = x(i)*/

  //start point
  {
    val (v, vi): (Int, Int) = ???
    val size: Int = ???
    val N: Int = ???
    val (n, ni): (Int, Int) = ???
    val (g, gi): (IM, IM) = ???
    val (s, si): (IM, IM) = ???
    val A: SPL = ???
    val input = new VC(size)

    val inner: SPL = sigmasum(vi - 1, new VC(n), (i: Int, in: VC, out: VC) => {
      Scatter(out, si, ni, n)(A(Gather(gi, ni, n)(in)))
    })
    val outer: SPL = sigmasum(v - 1, new VC(N), (i: Int, in: VC, out: VC) => {
      Scatter(out, s, n, N)(inner(Gather(g, n, N)(in)))
    })
    outer(input)
  }

  //introducing functions
  {
    val (v, vi): (Int, Int) = ???
    val size: Int = ???
    val N: Int = ???
    val (n, ni): (Int, Int) = ???
    val (g, gi): (IM, IM) = ???
    val (s, si): (IM, IM) = ???
    val A: SPL = ???
    val input = new VC(size)

    def innerf(): SPL = {
      sigmasum(vi - 1, new VC(n), (i: Int, in: VC, out: VC) => {
        Scatter(out, si, ni, n)(A(Gather(gi, ni, n)(in)))
      })
    }

    val outer: SPL = sigmasum(v - 1, new VC(N), (i: Int, in: VC, out: VC) => {
      Scatter(out, s, n, N)(innerf()(Gather(g, n, N)(in)))
    })
    outer(input)
  }

  //DFT64
  {
    def DFT2f(): SPL = ???

    def DFT32f(): SPL = ???

    case class DFT64() extends SPL {
      def apply(v: VC): VC = {
        val (k, m) = (2, 32)
        val afterperm = Perm(l(64, k))(v)
        val right: SPL = sigmasum(m - 1, new VC(64), (i: Int, in: VC, out: VC) => {
          Scatter(out, h(i * k, 1), k, in.size)(DFT2f()(Gather(h(i * k, 1), k, in.size)(in)))
        })
        val scale = diag(d())(right(afterperm))
        val left = sigmasum(k - 1, new VC(64), (i: Int, in: VC, out: VC) => {
          Scatter(out, h(i, k), k, in.size)(DFT32f()(Gather(h(i, k), k, in.size)(in)))
        })
        left(scale)
      }
    }
  }

  //fuse
  {
    val (v, vi): (Int, Int) = ???
    val size: Int = ???
    val N: Int = ???
    val (n, ni): (Int, Int) = ???
    val (g, gi): (IM, IM) = ???
    val (s, si): (IM, IM) = ???
    val A: SPL = ???
    val input = new VC(size)

    /*def innerf(): SPL = {
      sigmasum(vi - 1, new VC(n), (i: Int, in: VC, out: VC) => {
        Scatter(out, si, ni, n)(A(Gather(gi, ni, n)(in)))
      })
    }*/

    val outer: SPL = sigmasum(v - 1, new VC(N), (i: Int, in: VC, out: VC) => {
      def innerf(): SPL = {
        sigmasum(vi - 1, new VC(n), (i: Int, in: VC, out: VC) => {
          Scatter(out, IMC(si, s), ni, n)(A(Gather(IMC(gi, g), ni, n)(in)))
        })
      }

      innerf()(in)
    })
    outer(input)
  }

  //fuse without inner
  {
    val (v, vi): (Int, Int) = ???
    val size: Int = ???
    val N: Int = ???
    val (n, ni): (Int, Int) = ???
    val (gx, gi): (IM, IM) = ???
    val (sx, si): (IM, IM) = ???
    val A: SPL = ???
    val input = new VC(size)

    /*def innerf(): SPL = {
      sigmasum(vi - 1, new VC(n), (i: Int, in: VC, out: VC) => {
        Scatter(out, si, ni, n)(A(Gather(gi, ni, n)(in)))
      })
    }*/

    def innerf(s: IM, g: IM): SPL = {
      sigmasum(vi - 1, new VC(n), (i: Int, in: VC, out: VC) => {
        Scatter(out, IMC(si, s), ni, n)(A(Gather(IMC(gi, g), ni, n)(in)))
      })
    }

    val outer: SPL = sigmasum(v - 1, new VC(N), (i: Int, in: VC, out: VC) => {
      innerf(gx, sx)(in)
    })
    outer(input)
  }
  //dft with function
  {
    def DFT2f(): SPL = ???

    def DFT4f(out: VC, g: IM, s: IM): SPL = {
      case class DFT4() extends SPL {
        def apply(v: VC): VC = {
          val (k, m) = (2, 2)
          val afterperm = Perm(l(4, k))(v)
          val right: SPL = sigmasum(m - 1, new VC(4), (i: Int, in: VC, out: VC) => {
            Scatter(out, IMC(s, h(i * k, 1)), k, in.size)(DFT2f()(Gather(IMC(g, h(i * k, 1)), k, in.size)(in)))
          })
          val scale = diag(d())(right(afterperm))
          val left = sigmasum(k - 1, new VC(4), (i: Int, in: VC, out: VC) => {
            Scatter(out, IMC(s, h(i, k)), k, in.size)(DFT2f()(Gather(IMC(g, h(i, k)), k, in.size)(in)))
          })
          left(scale)
        }
      }
      DFT4()
    }

    case class DFT16() extends SPL {
      def apply(v: VC): VC = {
        val (k, m) = (4, 4)
        val afterperm = Perm(l(16, k))(v)
        val right: SPL = sigmasum(m - 1, new VC(4), (i: Int, in: VC, out: VC) => {
          DFT4f(out, h(i * k, 1), h(i * k, 1))(in)
        })
        val scale = diag(d())(right(afterperm))
        val left = sigmasum(k - 1, new VC(4), (i: Int, in: VC, out: VC) => {
          DFT4f(out, h(i, k), h(i, k))(in)
        })
        left(scale)
      }
    }
  }

  //symbolic
  {
    def dftf(n: Int): SPL = {
      case class DFT(n: Int) extends SPL {
        def apply(v: VC): VC = {
          if (v.size == 2) DFT2()(v) else {
            val k: Int = ???
            //choose radix
            val m = n / k
            val afterperm = Perm(l(n, k))(v)
            val right: SPL = sigmasum(m - 1, new VC(n), (i: Int, in: VC, out: VC) => {
              Scatter(out, h(i * k, 1), k, n)(dftf(k)(Gather(h(i * k, 1), k, in.size)(in)))
            })
            val scale = diag(d())(right(afterperm))
            val left = sigmasum(k - 1, new VC(n), (i: Int, in: VC, out: VC) => {
              Scatter(out, h(i, k), k, n)(dftf(k)(Gather(h(i, k), k, in.size)(in)))
            })
            left(scale)
          }
        }
      }
      DFT(n)
    }
  }


  //merged g/s
  {

    def dftfwithmerge(n: Int, g: IM, s: IM): SPL = {
      case class DFT(n: Int) extends SPL {
        def apply(v: VC): VC = {
          if (v.size == 2) DFT2()(v) else {
            val k: Int = ???
            //choose radix
            val m = n / k
            val afterperm = Perm(l(n, k))(v)
            val right: SPL = sigmasum(m - 1, new VC(n), (i: Int, in: VC, out: VC) => {
              Scatter(out, IMC(s, h(i * k, 1)), k, n)(dftf(k)(Gather(h(i * k, 1), k, in.size)(in)))
            })
            val scale = diag(d())(right(afterperm))
            val left = sigmasum(k - 1, new VC(n), (i: Int, in: VC, out: VC) => {
              Scatter(out, h(i, k), k, n)(dftf(k)(Gather(IMC(g, h(i, k)), k, in.size)(in)))
            })
            left(scale)
          }
        }
      }
      DFT(n)
    }

    def dftf(n: Int): SPL = {
      case class DFT(n: Int) extends SPL {
        def apply(v: VC): VC = {
          if (v.size == 2) DFT2()(v) else {
            val k: Int = ???
            //choose radix
            val m = n / k
            val afterperm = Perm(l(n, k))(v)
            val right: SPL = sigmasum(m - 1, new VC(n), (i: Int, in: VC, out: VC) => {
              Scatter(out, h(i * k, 1), k, n)(dftf(k)(Gather(h(i * k, 1), k, in.size)(in)))
            })
            val scale = diag(d())(right(afterperm))
            val left = sigmasum(k - 1, new VC(n), (i: Int, in: VC, out: VC) => {
              Scatter(out, h(i, k), k, n)(dftf(k)(Gather(h(i, k), k, in.size)(in)))
            })
            left(scale)
          }
        }
      }
      DFT(n)
    }
  }


  def choose_radix(n: Int): Int = ???

  def applyTwiddle(n: Int, k: Int, x: Array[Complex]): Unit = ???

  def applyPrecomputed(x: Array[Complex], y: Array[Complex]): Unit = ???

  def DFT2(x: Array[Complex], y: Array[Complex], idxf: Int => Int) = {
    y(idxf(0)) = x(idxf(0)) + x(idxf(1))
    y(idxf(1)) = x(idxf(0)) - x(idxf(1))
  }

  def ndix(idxf: Int => Int, i: Int): Int => Int = ???

  def ndix2(idxf: Int => Int, i: Int): Int => Int = ???

  def permute(x: Array[Complex]): Unit = ???

  def T(n: Int, k: Int, i: Int): Complex = ???

  def DFT(n: Int, x: Array[Complex], y: Array[Complex], idxf: Int => Int): Unit = {
    if (n == 2) DFT2(x, y, idxf) else {
      val k = choose_radix(n)
      permute(x)
      val buffer = new Array[Complex](n)
      for (i <- 0 until k) DFT(n / k, x, buffer, ndix(idxf, i))
      applyTwiddle(n, k, buffer)
      for (i <- 0 until k) DFT(k, buffer, y, ndix2(idxf, i))
    }
  }


  def DFT4(x: Array[Complex], y: Array[Complex]): Unit = {
    //val k = 2
    permute(x)
    val buffer = new Array[Complex](4)
    //i == 0; DFT(4/2,x,y,nidx(0)), idxf(0) = 0, idxf(1) = 1
    buffer(0) = x(0) + x(1)
    buffer(1) = x(0) - x(1)
    //i == 1; DFT(4/2,x,y,nidx(1)), idxf(0) = 2, idxf(1) = 3
    buffer(2) = x(2) + x(3)
    buffer(3) = x(2) - x(3)
    //applyTwiddle(4, 2, buffer)
    //buffer(0) = buffer(0) * T(4,2,0); T(4,2,0) == Complex(1,0)
    //buffer(1) = buffer(1) * T(4,2,1); T(4,2,1) == Complex(1,0)
    //buffer(2) = buffer(2) * T(4,2,2); T(4,2,2) == Complex(1,0)
    //buffer(3) = buffer(3) * T(4,2,3); T(4,2,3) == Complex(0,-1)
    buffer(3) = buffer(3) * Complex(0, -1);

    //i == 0; DFT(4/2,x,y,nidx2(0)), idxf(0) = 0, idxf(1) = 2
    y(0) = buffer(0) + buffer(2)
    y(2) = buffer(0) - buffer(2)
    //i == 1; DFT(4/2,x,y,nidx2(1)), idxf(0) = 1, idxf(1) = 3
    y(1) = buffer(1) + buffer(3)
    y(3) = buffer(1) - buffer(3)
  }

  {

    def DFT64(x: Array[Complex], y: Array[Complex], idxf: Int => Int): Unit = ???

    def DFT63(x: Array[Complex], y: Array[Complex], idxf: Int => Int): Unit = ???

    def DFT62(x: Array[Complex], y: Array[Complex], idxf: Int => Int): Unit = ???

    def DFT61(x: Array[Complex], y: Array[Complex], idxf: Int => Int): Unit = ???

    def DFT(n: Int, x: Array[Complex], y: Array[Complex], idxf: Int => Int): Unit = {
      if (n <= 64) {
        if (n == 64) DFT64(x, y, idxf)
        if (n == 63) DFT63(x, y, idxf)
        if (n == 62) DFT62(x, y, idxf)
        if (n == 61) DFT62(x, y, idxf)
      }
      else {
        val k = choose_radix(n)
        permute(x)
        val buffer = new Array[Complex](n)
        for (i <- 0 until k) DFT(n / k, x, buffer, ndix(idxf, i))
        applyTwiddle(n, k, buffer)
        for (i <- 0 until k) DFT(k, buffer, y, ndix2(idxf, i))
      }
    }
  }
  type Rep[T] = T

  case class Sym(n: Int) {
    val a: Int = 3
    val b: Int = 4
    val c = a + b
  }
  {
    val a: Rep[Int] = ???
    val b: Rep[Int] = ???
    val c = a + b
  }
  {
    case class Complex(re: Rep[Double], im: Rep[Double]) {
      def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)

      def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)

      def *(that: Complex) = Complex(this.re * that.re - this.im * that.im,
        this.re * that.im + this.im * that.re)
    }
  }

  def myadd_Int(x: Int, y: Int): Int = x + y
  def myadd_Float(x: Float, y: Float): Float = x + y

  def myadd[T: Numeric](x: T, y: T): T = {
    val ev = implicitly[Numeric[T]]
    import ev._
    x + y
  }

  def myadd_now(x: Int, y: Int): Int = x + y
  def myadd_later(x: Rep[Int], y: Rep[Int]): Rep[Int] = x + y


  val scalars: Array[Rep[Double]] = ???
  scalars(0) = scalars(1)

  val sarray: Rep[Array[Double]] = ???
  sarray(0) = sarray(1)

  for (i <- 0 until scalars.size)
    scalars(i) = sarray(i)

  val i: Rep[Int] = ???
  scalars(i) = sarray(i)


  abstract class SchroedingersArray[T]{
    def apply(i: Int): T
    def update(i: Int,y: T)
  }

  case class SArray(data: Rep[Array[Double]]) extends SchroedingersArray[Rep[Double]]{
    def apply(i: Int) = data(i)
    def update(i: Int,y: Double) = data.update(i,y)
  }

  case class ScalarArray(data: Array[Rep[Double]]) extends SchroedingersArray[Rep[Double]]{
    def apply(i: Int) = data(i)
    def update(i: Int,y: Double) = data.update(i,y)
  }


  def stagefunction(f: Rep[Int] => Rep[Int]): Rep[Int => Int] = ???

  def fun_factory(stat: Int): (Rep[Int] => Rep[Int]) = {
    val f: (Rep[Int] => Rep[Int]) = (dyn: Rep[Int]) =>{
      stat + dyn
    }
    stagefunction(f)
  }

  type Stat = Int
  type Dyn = Int
  case class Mix(stat: Stat, dyn: Dyn)

  def fun_factory2(stat: Stat): (Dyn => Rep[Int]) = {
    val f: (Dyn => Rep[Int]) = (dyn: Dyn) =>{
      val mix = Mix(stat,dyn)
      ???
    }
    stagefunction(f)
  }


  /*val scalaf: Rep[Int] => Rep[Int] = ???
  val stagedf: Rep[Int => Int] = ???

  case class MaybeSFunction(f: Either[Rep[Int => Int], Rep[Int] => Rep[Int]])

  def loop(until: Int, f: Int => Int) = ???
  def stagedloop(until: Rep[Int], f: Rep[Int] => Rep[Int]) = ???

  def genloop[T](until: T, f: T => T) = ???

  def getTwiddle(n: Int, k: Int): Complex = ???
  def getTwiddle(n: Rep[Int], k: Rep[Int]): Rep[Complex] = ???
  def getTwiddle[R[_]](n: R[Int], k: R[Int]): R[Complex] = ???*/

/*
  def Function[A, B, C](a: A, b: B, c: C) = ???

  def specializeDFT[T,A,R](n: T): Any=>Any = {
    case n: Int =>{
      val f: (Array[Complex]) => Array[Complex] = (args: Array[Complex]) => {
        val size = n
      }
      f
    }
    case n: Rep[Int] => {
      val f: (Int,Array[Complex]) => Array[Complex] = (args: (Int,Array[Complex])) => args._2
      f
    }
  }*/


  /*def run2meta(n: Rep[Int]): Unit ={
    if (n == 2){
      val metaKnowledge_n = 2
      DFT(metaKnowledge_n,...)
    } else {
      DFT(n,...)
    }
  }*/




}
