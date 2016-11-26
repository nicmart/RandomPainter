package paint

/**
  * Created by nic on 26/11/2016.
  */
object Geometry
{
    case class Point(x: Int, y: Int) {
        def +(other: Point) = Point(x + other.x, y + other.y)
        def -(other: Point) = Point(x - other.x, y - other.y)
        def *(d: Double) = Point((x * d).toInt, (y * d).toInt)
        def *(d: Int) = Point(x * d, y * d)
        def /(d: Double) = Point((x / d).toInt, (y / d).toInt)
        def /(d: Int) = Point(x / d, y / d)
    }

    case class DoublePoint(x: Double, y: Double) {
        def +(other: DoublePoint) = DoublePoint(x + other.x, y + other.y)
        def -(other: DoublePoint) = DoublePoint(x - other.x, y - other.y)
        def *(d: Double) = DoublePoint(x * d, y * d)
        def *(d: Int) = DoublePoint(x * d, y * d)
        def /(d: Double) = DoublePoint(x / d, y / d)
        def /(d: Int) = DoublePoint(x / d, y / d)
    }

    type PlaneTransformation = DoublePoint => DoublePoint
}
