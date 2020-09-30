package Basics

object ClassesAndTraits extends App {

  abstract class Vector
  abstract class Shape extends Movable

  final case class Vector2(x: Double, y: Double) extends Vector
  {
    def + (vector2: Vector2): Vector2 = new Vector2(this.x + vector2.x, this.y + vector2.y)
    def + (length: Double): Vector2 = new Vector2(this.x + length, this.y + length)
    def - (vector2: Vector2): Vector2 = new Vector2(this.x - vector2.x, this.y - vector2.y)
    def - (length: Double): Vector2 = new Vector2(this.x - length, this.y - length)
    def / (divider: Double): Vector2 = new Vector2(this.x / divider, this.y / divider)
  }

  final case class Vector3(x: Double, y: Double, z: Double) extends Vector
  {
    def + (vector3: Vector3): Vector3 = new Vector3(this.x + vector3.x, this.y + vector3.y, this.z + vector3.z)
    def + (length: Double): Vector3 = new Vector3(this.x + length, this.y + length, this.z + length)
    def - (vector3: Vector3): Vector3 = new Vector3(this.x - vector3.x, this.y - vector3.y, this.z - vector3.z)
    def - (length: Double): Vector3 = new Vector3(this.x - length, this.y - length, this.z - length)
    def / (divider: Double): Vector3 = new Vector3(this.x / divider, this.y / divider, this.z / divider)
    def * (vector3: Vector3): Vector3 = new Vector3(this.x * vector3.x, this.y * vector3.y, this.z * vector3.z)
    def * (multiplier: Double): Vector3 = new Vector3(this.x * multiplier, this.y * multiplier, this.z * multiplier)
  }

  object Vector3{
    val Zero = Vector3(0.0, 0.0, 0.0)
    val One = Vector3(1.0, 1.0, 1.0)
    val Up = Vector3(0.0, 1.0, 0.0)
    val Down = Vector3(0.0, -1.0, 0.0)
    val Right = Vector3(1.0, 0.0, 0.0)
    val Left = Vector3(-1.0, 0.0, 0.0)
    val Forward = Vector3(0.0, 0.0, -1.0)
    val Backward = Vector3(0.0, 0.0, 1.0)
  }

  sealed trait Location[A] { def getLocation: A }
  sealed trait Bounded[A]  { def getBounds: A }
  sealed trait Movable { def move(Translate: Vector): Shape }
  sealed trait MathFields2 { def area: Double}
  sealed trait MathFields3 {
    def surfaceArea: Double
    def volume: Double
  }

  final case class Point2(Location: Vector2) extends Shape with Location[Vector2] with Bounded[(Vector2, Vector2)]
  {
    override def getLocation: Vector2 = Location
    override def getBounds: (Vector2, Vector2) = (getLocation, getLocation)
    override def move(Translate: Vector): Point2 = Point2(getLocation + Translate.asInstanceOf[Vector2])
  }

  final case class Point3(Location: Vector3) extends Shape with Location[Vector3] with Bounded[(Vector3, Vector3)]
  {
    override def getLocation: Vector3 = Location
    override def getBounds: (Vector3, Vector3) = (getLocation, getLocation)
    override def move(Translate: Vector): Point3 = Point3(getLocation + Translate.asInstanceOf[Vector3])
  }

  final case class Circle(Location: Vector2, radius: Double) extends Shape with Location[Vector2] with Bounded[(Vector2, Vector2)] with MathFields2
  {
    override def getLocation: Vector2 = Location
    override def getBounds: (Vector2, Vector2) = (getLocation - radius, getLocation + radius)
    override def move(Translate: Vector): Circle = Circle(getLocation + Translate.asInstanceOf[Vector2], radius)
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Sphere(Location: Vector3, radius: Double) extends Shape with Location[Vector3] with Bounded[(Vector3, Vector3)] with MathFields3
  {
    override def getLocation: Vector3 = Location
    override def getBounds: (Vector3, Vector3) = (getLocation - radius, getLocation + radius)
    override def move(Translate: Vector): Sphere = Sphere(getLocation + Translate.asInstanceOf[Vector3], radius)
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = (4.0/3.0) * Math.PI * Math.pow(radius, 3)
  }

  final case class Rectangle(Location: Vector2, Size: Vector2) extends Shape with Location[Vector2] with Bounded[(Vector2, Vector2)]  with MathFields2
  {
    override def getLocation: Vector2 = Location
    override def getBounds: (Vector2, Vector2) = (getLocation - Size / 2, getLocation + Size / 2)
    override def move(Translate: Vector): Rectangle = Rectangle(getLocation + Translate.asInstanceOf[Vector2], Size)
    override def area: Double = Size.x * Size.y
  }

  final case class Cuboid(Location: Vector3, Size: Vector3) extends Shape with Location[Vector3] with Bounded[(Vector3, Vector3)] with MathFields3
  {
    override def getLocation: Vector3 = Location
    override def getBounds: (Vector3, Vector3) = (getLocation - Size / 2 , getLocation + Size / 2)
    override def move(Translate: Vector): Cuboid = Cuboid(getLocation + Translate.asInstanceOf[Vector3], Size)
    // Total surface area
    override def surfaceArea: Double = 2 * (Size.x * Size.z + Size.z * Size.y + Size.y * Size.x)
    override def volume: Double = Size.x * Size.y * Size.z
  }

  final case class Square(Location: Vector2, Size: Double) extends Shape with Location[Vector2] with Bounded[(Vector2, Vector2)] with MathFields2
  {
    override def getLocation: Vector2 = Location
    override def getBounds: (Vector2, Vector2) = (getLocation - Size / 2, getLocation + Size / 2)
    override def move(Translate: Vector): Square = Square(getLocation + Translate.asInstanceOf[Vector2], Size)
    override def area: Double = Math.pow(Size, 2)
  }

  final case class Cube(Location: Vector3, Size: Double) extends Shape with Location[Vector3] with Bounded[(Vector3, Vector3)] with MathFields3
  {
    override def getLocation: Vector3 = Location
    override def getBounds: (Vector3, Vector3) = (getLocation - Size / 2 , getLocation + Size / 2)
    override def move(Translate: Vector): Cube = Cube(getLocation + Translate.asInstanceOf[Vector3], Size)
    override def surfaceArea: Double = 6 * Math.pow(Size, 2)
    override def volume: Double = Math.pow(Size, 3)
  }

  final case class Triangle(Location: Vector2, Size: Vector2) extends Shape with Location[Vector2] with Bounded[(Vector2, Vector2)] with MathFields2
  {
    override def getLocation: Vector2 = Location
    override def getBounds: (Vector2, Vector2) = (getLocation - Size / 2, getLocation + Size / 2)
    override def move(Translate: Vector): Triangle = Triangle(getLocation + Translate.asInstanceOf[Vector2], Size)
    override def area: Double = (Size.x * Size.y) / 2
  }

  final case class Triangle3(Location: Vector3, Size: Vector3) extends Shape with Location[Vector3] with Bounded[(Vector3, Vector3)] with MathFields3
  {
    override def getLocation: Vector3 = Location
    override def getBounds: (Vector3, Vector3) = (getLocation - Size / 2, getLocation + Size / 2)
    override def move(Translate: Vector): Triangle3 = Triangle3(getLocation + Translate.asInstanceOf[Vector3], Size)
    override def surfaceArea: Double = ???
    override def volume: Double = ???
  }

  final case class Model(Location: Vector3, Vertices: List[Vector3]) extends Shape with Location[Vector3] with Bounded[(Vector3, Vector3)] with MathFields3
  {
    override def getLocation: Vector3 = Location
    override def getBounds: (Vector3, Vector3) = (
      Vector3(
        Vertices.map(_.x + getLocation.x).min,
        Vertices.map(_.y + getLocation.y).min,
        Vertices.map(_.z + getLocation.z).min
      ),
      Vector3(
        Vertices.map(_.x + getLocation.x).max,
        Vertices.map(_.y + getLocation.y).max,
        Vertices.map(_.z + getLocation.z).max
      )
    )
    override def move(Translate: Vector): Model = Model(getLocation + Translate.asInstanceOf[Vector3], Vertices)
    override def surfaceArea: Double = ???
    override def volume: Double = ???

  }


  val shapes: List[Shape] = List(Point2(Vector2(2.0, 1.0)), Point3(Vector3(5.0, 6.0, 7.0)))
  val shapes2: List[Shape] = Cube(Vector3(2.0, 1.0, 0.0), 1.0) :: Sphere(Vector3.One, 3.0) :: Nil
  val shapes2moved = for (shape <- shapes2) yield shape.move(Vector3.One)
  println(shapes2moved)

  val planeModel = Model(
    Vector3.Zero,
    // Clockwise
    Vector3.Zero :: Vector3.Up :: Vector3.Right ::
    Vector3.Right :: Vector3.Up :: Vector3.Up + Vector3.Right ::
    Nil
  )
  println(planeModel.getBounds)

  val planeModelMoved = planeModel.move(Vector3.Right * 4.0)
  println(planeModelMoved.getBounds)

  val shapes3 = planeModel :: shapes2
  println(shapes3)
}
