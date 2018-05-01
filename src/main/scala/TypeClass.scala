
case class Person(name: String, age: Int)
//  extends Serializable[Person]{
//  def serialize: String = s"$name->$age"
//}
case class Resturant(name:String, brunch: Boolean)
//  extends Serializable[Resturant] {
//  def serialize = s"$name -> $brunch"
//}

trait Serializable[A] {
  def serialize(x:A): String
}

trait customOrdering[A] {
  def compare(first: A, second: A): Int
}

object typeClass extends App {

//  val x = Person("aasih", 21).serialize
   def serialize[T](x: T)(implicit ser: Serializable[T]): String = {
     ser.serialize(x)
   }

  def findMax[T](x: List[T])(implicit ord: customOrdering[T]): T = {
    x.reduce((a, b) => if (ord.compare(a, b) > 0) a else b)
  }

  implicit class zx(x: Person) {
    implicit object PerSonIsSerial extends Serializable[Person] {
      def serialize(x: Person): String = s"${x.name}"
    }
    def serila: String = PerSonIsSerial.serialize(x)
  }

  implicit object PerSonIsSerial extends Serializable[Person] {
    def serialize(x: Person): String = s"${x.name}"
  }


  implicit object PerSonIsOrder extends customOrdering[Person] {
    def compare(first: Person, second: Person): Int = {
      first.age - second.age
    }
  }

  implicit object ResturantIsSerial extends Serializable[Resturant] {
    def serialize(x: Resturant): String = s"${x.name}-->${x.brunch}"
  }

  implicit object ListIsSerial extends Serializable[List[Person]] {
    def serialize(x: List[Person]): String = x.toString()
  }

  implicit def listIsSerial[T: Serializable] = new Serializable[List[T]] {
    override def serialize(x: List[T]): String = x.toString}

  println(serialize(Person("aashish", 2)))

  println(serialize(List(Person("aashish", 2),Person("bagla", 1))))

  println(findMax(List(Person("aashish", 2),Person("bagla", 1), Person("bagla", 33))))

  println(serialize(Resturant("aashish", true)))

  val xx = Person("aashish", 2).serila
  println(xx)
}