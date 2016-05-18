/**
  * Created by tmanassis on 18/05/2016.
  */

import scala.math._


object myvectors extends App{







  case class Vector2d( x: Double, val y: Double) {


    override def toString(): String = "(" + x + ", " + y + ")";



    // vector magnitude
    def magnitude (): Double = math.sqrt(x*x+y*y)




    // vector/ scalar operations

    def +(a: Float) = Vector2d(x + a, y + a)
    def -(a: Float) = Vector2d(x - a, y - a)
    def *(a: Float) = Vector2d(x * a, y * a)
    def /(a: Float) = Vector2d(x / a, y / a)

    def cross(a: Float) = Vector2d(a * y, -a * x)


    // vector/ vector operations


    def +(v: Vector2d) = Vector2d(x + v.x, y + v.y)
    def -(v: Vector2d) = Vector2d(x - v.x, y - v.y)

    def dot(v: Vector2d) = x * v.x + y * v.y
    def cross(v: Vector2d) = x * v.y - y * v.x



  }


  val pt = new Vector2d(1,2)
  val pt2 = new Vector2d(5,5)



  val dotted =  pt.dot(pt2)
  val crossed = pt.cross(pt2)
  val plussed = pt.+(pt2) + 3


  println("v1"+pt)
  println("v2"+pt2)
  println(pt2.magnitude())
  println(dotted)
  println(crossed)
  println(plussed)








  //#val now = new Date()

  //#println(now)




}
