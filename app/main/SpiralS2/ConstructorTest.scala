package SpiralS2

/**
  * Created by rayda on 12-Jan-17.
  */


class Parent(i: Int = 3){

  val x = i
  def blub() = {
    println(s"$i  $x")
  }
}

class Child(i: Int = 0) extends Parent(i)


object ConstructorTest extends App{
  val t = new Child()
  t.blub()


}
