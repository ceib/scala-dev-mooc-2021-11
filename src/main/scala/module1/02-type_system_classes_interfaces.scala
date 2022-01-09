package module1
import java.io.Closeable
import java.io.File
import java.io.InputStream
import scala.io.BufferedSource
import java.io.InputStreamReader
import scala.io.Source

object type_system extends App {

  /**
   * Scala type system
   *
   */

   // AnyVal

   //


   // Unit



   // Null


   // Nothing

  def absurd(v: Nothing) = ???


  // Generics

  // работа с ресурсом

 lazy  val file : File = ???

 lazy val source: BufferedSource = Source.fromFile(file)

 lazy val lines: Iterator[String] = source.getLines()

 // source.close()

 lazy val lines2 = try{
     source.getLines()
 } finally {
     source.close()
 }

 def ensureClose[S <: Closeable, R](source: S)(f: S => R): R = {
     try{
         f(source)
     } finally {
         source.close()
     }
 }

 lazy val lines3: Iterator[String] = ensureClose(Source.fromFile(file))(_.getLines())
  
  

  // ограничения связанные с дженериками

  def print(seq: Seq[Int]): Unit = ???

  lazy val seq1: Seq[Int] = ???
  lazy val seq2: Seq[String] = ???


  lazy val seq = Seq(1, 2, 3)

  seq.isInstanceOf[Seq[Int]]

  /**
   *
   * class
   *
   * конструкторы / поля / методы / компаньоны
   */

   class User private (email: String, password: String){

     def this(email: String) = this(email, "vjhbfvhjbvfh")

   }

   object User{
       def from(e: String, p: String) = new User(e, p)
   }



  // val u: User = new User("foo", "bar")
  // val u2: User = new User("foo")


  val u = User.from("foo", "bar")
  val u2 = User.from("foo", "bar")




  /**
   * Задание 1: Создать класс "Прямоугольник"(Rectangle), мы должны иметь возможность создавать прямоугольник с заданной
   * длиной(length) и шириной(width), а также вычислять его периметр и площадь
   * 
   */

    case class Rectangle(length: Int, width: Int){
        def p: Int = (length + width) * 2
        def s: Int = length * width
    }
   



  /**
   * object
   *
   * 1. Паттерн одиночка
   * 2. Линивая инициализация
   * 3. Могут быть компаньоны
   */




  /**
   * case class
   *
   */


   // создать case класс кредитная карта с двумя полями номер и cvc

  case class CreditCard(number: String, cvc: String)




  /**
   * case object
   *
   * Используются для создания перечислений или же в качестве сообщений для Акторов
   */

  //enum
  sealed trait Language
  case object Russian extends Language
  case object Estonian extends Language
  case object French extends Language

  //usage enum
  case class Book(name: String,
                  original: Language,
                  translated: Seq[Language])

  /**
   * trait
   *
   */

   type UserId = String

   trait UserService{
       def get(id: String): User
       def insert(user: User): Unit
       def id: String
   }

   trait WithId{
       def typedId: UserId
   }

   class UserImpl extends UserService {
       def get(id: String): User = ???
       
       def insert(user: User): Unit = ???

       def typedId: UserId = ???
       val id: String = ???
       
   }

   lazy val ui = new UserImpl with WithId





  class A {
    def foo() = "A"
  }

  trait B extends A {
    override def foo() = "B" + super.foo()
  }

  trait C extends B {
    override def foo() = "C" + super.foo()
  }

  trait D extends A {
    override def foo() = "D" + super.foo()
  }

  trait E extends C {
    override def foo(): String = "E" + super.foo()
  }

  val v = new A with D with C with B // CBDA

   // A -> D -> B -> C

  val v1 = new A with E with D with C with B // DECBA
  // A -> B -> C -> E -> D     //DECBA







  /**
   * Value classes и Universal traits
   */

  class Meter(val value: Double) extends AnyVal {
    def +(m: Meter): Meter = new Meter(value + m.value)
  }
  val x = new Meter(3.4)
  val y = new Meter(4.3)
  val z = x + y
  println(z.value)

  trait Printable extends Any {
    def print(): Unit = println(this)
  }
  class Wrapper(val underlying: Int) extends AnyVal with Printable

  val w = new Wrapper(3)
  w.print() // actually requires instantiating a Wrapper instance
}