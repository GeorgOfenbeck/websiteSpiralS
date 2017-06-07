/**
  * Created by rayda on 16-Mar-17.
  */
object LTest extends App {
  def L(n: Int, k: Int) {
      for (i <- 0 until n)
        println(s"${(i / (n / k) + k * (i % (n / k)))} => $i ")
    }

  L(8,4)
}
