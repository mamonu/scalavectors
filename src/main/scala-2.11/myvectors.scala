/**
  * Created by tmanassis on 18/05/2016.
  */

import scala.math._


object myvectors extends App{







  class Vector2d( x: Double, val y: Double) {


    override def toString(): String = "(" + x + ", " + y + ")";

    def magnitude (): Double = math.sqrt(x*x+y*y)


  }


  val pt = new Vector2d(1,2)
  val pt2 = new Vector2d(5,5)






  println(pt)
  println(pt2)
  println(pt2.magnitude())






  //#val now = new Date()

  //#println(now)




}
