package module2

import module1.list

object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  trait JsonWriter[T] {
    def write(v: T): JsValue
  }

  object JsonWriter {

    def apply[T](implicit ev: JsonWriter[T]) = ev

    import JsValue._

    implicit val strToJsValue = new JsonWriter[String] {
      def write(v: String): JsValue = JsString(v)
    }

    implicit val intToJsValue = new JsonWriter[Int] {
      def write(v: Int): JsValue = JsNumber(v)
    }

    implicit def optToJsValue[A](implicit
        ev: JsonWriter[A]
    ): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
      def write(v: Option[A]): JsValue = v match {
        case Some(value) => ev.write(value)
        case None        => JsNull
      }
    }

  }

  implicit class JsonSyntax[T](v: T) {
    def toJson(implicit ev: JsonWriter[T]) = ev.write(v)
  }

  def toJson[T: JsonWriter](v: T): JsValue = {
    //val jw = implicitly[JsonWriter[T]]
    JsonWriter[T].write(v)
  }

  toJson("dvfvfdbv")
  toJson(12)

  toJson(Option("abc"))
  toJson(Option(1))

  "dvfvfdbv".toJson
  12.toJson

  // 1
  trait Ordering[T] {
    def less(a: T, b: T): Boolean
  }

  object Ordering {
    // 2
    implicit val intOrdering = from[Int]((a, b) => a < b)

    implicit val strOrdering = from[String]((a, b) => a < b)

    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      def less(a: A, b: A): Boolean = f(a, b)
    }
  }

  implicit class OrderingOps[T](a: T) {
    def max2(b: T)(implicit ord: Ordering[T]): T = ???
  }

  // 3
  def max2[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if (ordering.less(a, b)) b else a

  5 max2 10
  max2(5, 10)
  max2("ab", "abc")

  //val result = List("a", "b", "c").filter(str => str === 1)

  trait Eq[T] {
    def ===(a: T, b: T): Boolean
  }

  object Eq {

    def apply[T](implicit ev: Eq[T]): Eq[T] = ev

    implicit val eqStr: Eq[String] = new Eq[String] {
      def ===(a: String, b: String): Boolean = a == b
    }
  }

  implicit class EqSyntax[T](a: T) {
    def ===(b: T)(implicit eq: Eq[T]): Boolean = eq.===(a, b)
  }



 /**
   * 
   * Опциональное ДЗ
   * Доработать Bindable и метод tupleF под type class pattern
   */

  def tupleF[F[_], A, B](fa: F[A], fb: F[B]) = ???

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

   val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))

}
