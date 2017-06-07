/*
package TinyDFT
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
/**
  * Created by rayda on 07-Nov-16.
  */

class TinyFFT extends sort.Skeleton {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with sort.ScalaGenSort_DSL with JavaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

/*
  def f2(in: Rep[Array[Double]], out: Rep[Array[Double]]): Rep[Array[Double]] = {
    val t1 = vector_apply(in,Const(0))
    val t2 = vector_apply(in,(Const(1)))
    val r1 = genplus(t1,t2)
    val r2 = genplus(t1,t2) //replace me with minus
    vector_obj_fromseq(Vector(r1,r2))
  }

  def dft(n: Int, in: Rep[Array[Double]], out: Rep[Array[Double]])= {
    if (n == 2)
      f2(in,out)
    else {
      val k = choose_factor(n)
      for (i <- 0 until k) dft(n/k,in, out)
      for (i <- 0 until n) out(i) = twiddle(i) * out(i)
      for (i <- 0 until n/k) dft_strided(k,out,out)
    }
  }

  def dft(n: Int, in: Rep[Array[Double]], out: Rep[Array[Double]]) = ???

  def twiddle(i: Int): Rep[Double] = ???
  def choose_factor(n: Int): Int = ???*/
}




object KnownVsUnkown extends App {



}
*/
