/**
  * Created by tmanassis on 18/05/2016.
  */

import scala.math._



object myvectors extends App{


  case class Vector2d( x: Double, val y: Double) {


    override def toString(): String = "(" + x + ", " + y + ")";


    // vector/ scalar operations

    def +(a: Float) = Vector2d(x + a, y + a)
    def -(a: Float) = Vector2d(x - a, y - a)
    def *(a: Float) = Vector2d(x * a, y * a)
    def /(a: Float) = Vector2d(x / a, y / a)

    def cross(a: Float) = Vector2d(a * y, -a * x)


    // vector/ vector operations

    def distance(v: Vector2d) = {
      val dx = x - v.x
      val dy = y - v.y
      sqrt(dx * dx + dy * dy)
    }

    // vector magnitude
    def magnitude (): Double = math.sqrt(x*x+y*y)


    def isCoincident(v: Vector2d): Boolean = {
      distance(v) < 0.0001
    }




    def +(v: Vector2d) = Vector2d(x + v.x, y + v.y)
    def -(v: Vector2d) = Vector2d(x - v.x, y - v.y)

    def dot(v: Vector2d) = x * v.x + y * v.y
    def cross(v: Vector2d) = x * v.y - y * v.x




    def rotate(angle: Double) = {
      val rad = toRadians(angle)
      val sa = sin(rad)
      val ca = cos(rad)
      Vector2d(x * ca - y * sa, y * ca + x * sa)
    }

    def calculateRotation(v: Vector2d) = {
      toDegrees(atan2((v.x - x), (v.y - y)))
    }



    def calculateRotation() = {
      toDegrees(atan2((x), (y)))
    }






  }

  case class Line(p1: Vector2d, p2: Vector2d, intensity: Double) {
    def this(p1: Vector2d, p2: Vector2d) = this(p1, p2, 1)

    def isCoincident(p: Vector2d): Boolean =
      p1.isCoincident(p) || p2.isCoincident(p)

    def isCoincident(otherLine: Line): Boolean = {
      p1.isCoincident(otherLine.p1) && p2.isCoincident(otherLine.p2) ||
        p1.isCoincident(otherLine.p2) && p2.isCoincident(otherLine.p1)
    }

  }


  class Rectangle(p1: Vector2d, p2: Vector2d) {
    var left = min(p1.x, p2.x)
    var top = min(p1.y, p2.y)
    var bottom = max(p1.y, p2.y)
    var right = max(p1.x, p2.x)

    def intersects(r2: Rectangle) =
      !(r2.left > right ||
        r2.right < left ||
        r2.top > bottom ||
        r2.bottom < top)

    def render() = {
      List(new Line(Vector2d(left, top), Vector2d(right, top)),
        new Line(Vector2d(right, top), Vector2d(right, bottom)),
        new Line(Vector2d(right, bottom), Vector2d(left, bottom)),
        new Line(Vector2d(left, bottom), Vector2d(left, top)))
    }

  }







  var pt = new Vector2d(0,0)
  var pt2 = new Vector2d(1,1)



  val dotted =  pt.dot(pt2)
  val crossed = pt.cross(pt2)
  val plussed = pt.+(pt2) + 3


  println("v1"+pt)
  println("v2"+pt2)
  println("magnitude of v2"+pt2.magnitude())
  println("dot product of 2 vectors"+dotted)
  println(crossed)
  println(plussed)

  val pt3=pt.rotate(90)

      pt = pt.rotate(45)


  println(pt.calculateRotation())
  println(pt2.calculateRotation())
  println(pt3.calculateRotation())


val rect = new Rectangle(pt,pt2)



    println(rect.render())
  //#val now = new Date()

  //#println(now)




}



