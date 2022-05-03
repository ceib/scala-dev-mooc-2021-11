package module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */
object referential_transparency {


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService{

    override def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient = {
      val abiturient = Abiturient(uuid, abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

  }
}


 // recursion

object recursion extends App  {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

   def fact(n: Int): Int = {
       var _n = 1
       var i = 2
       while(i <= n){
           _n *=  i
           i += 1
       }
       _n
   }


   def factRec(n: Int): Int = {
    if (n<=0) 1 else n * factRec(n-1) 
   }

 
   def factTailRec(n: Int): Int = {
       @tailrec
       def loop(n: Int, accum: Int): Int = 
           if(n == 1) accum
           else loop(n - 1, n * accum)
        loop(n, 1)
   }

  


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */
  //наивная реализация, для прогрева процессора :)
  def fib0(n: Int): Long = n match {
    case 0 => 0
    case 1 => 1
    case x => fib(x - 1) + fib(x - 2)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(si: Int, si_1: Int, n: Int): Int = {
      if (n == 1) si
      else loop(si + si_1, si, n-1)
    }
    if (n == 0) 0 else loop(1, 0, n)
  }

  def fib(n: Int): Long = {
    @tailrec
    def fibNext(x: Int, m1: Long, m2: Long): Long = x match {
      case 0 => m2
      case 1 => m1
      case _ => fibNext(x-1, m1 + m2, m1)
    }
    fibNext(n, 1, 0)
  }

  println( s" ${fib0(10)}") //55
  println( s" ${fib(50)}") //12586269025
}

object hof {

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

   def createRequestSubscription() = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe("request")

       stream.foreach{ rec =>
            val req: Request = Request.parse(rec.value)
            // save(request)
       }
   }

   def createSubscription[T](topic: String, action: Record => T): Unit = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe(topic)
       stream.foreach{ rec =>
            action(rec)
       }
   }
   def createRequestSubscription2 = createSubscription("request", r => {
       val req: Request = Request.parse(r.value)
            // save(request)
   })
  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = { a =>
        val start = System.currentTimeMillis()
        val result = f(a)
        val end = System.currentTimeMillis()
        println(end - start)
        result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  
   
  def not[A](f: A => Boolean): A => Boolean = a => ! f(a) 
  
  
  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)

  res(3) 


}






/**
 *  Реализуем тип Option
 */


 object opt extends App  {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+T]{
       def isEmpty: Boolean = this match {
           case Option.Some(v) => false
           case Option.None => true
       }

       def get: T = this match {
           case Option.Some(v) => v
           case Option.None => throw new Exception("Get on empty option")
       }

       def map[B](f: T => B): Option[B] = this match {
           case Option.Some(v) => Option.Some(f(v))
           case Option.None => Option.None
       }

       def flatMap[B](f: T => Option[B]): Option[B] = this match {
           case Option.Some(v) => f(v)
           case Option.None => Option.None
       }

      /**
       *
       * Реализовать метод printIfAny, который будет печатать значение, если оно есть
       */
       def printIfAny: Unit = this match {
          case Option.Some(v) => println(v)
          case Option.None =>
        }

      /**
       *
       * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
       */
      def zip[B](b: Option[B]): Option[(T, B)] = {
        if (!this.isEmpty && !b.isEmpty) Option(this.get, b.get)
        else Option.None
      }

      /**
       *
       * Реализовать метод filter, который будет возвращать не пустой Option
       * в случае если исходный не пуст и предикат от значения = true
       */
      def filter(predicate: Option[T] => Boolean): Option[T] = {
        if (this.isEmpty || !predicate(this)) Option.None
        else Option(this.get)
      }
   }

  object Option {
    case class Some[T](v: T) extends Option[T]

    case object None extends Option[Nothing]

    def apply[T](v: T): Option[T] = v match {
      case null => Option.None
      case _ => Option.Some(v)
    }

  }

  val x1 = Option(null)
  val x2 = Option(5)

  x1.printIfAny
  x2.printIfAny

  Option(5).zip(Option(6)).printIfAny
  Option(5).zip(Option(null)).printIfAny

  val predicateIntLess10: Option[Int] => Boolean = {
    case Option.Some(a) => a < 10
  }

  Option(5).filter(predicateIntLess10).printIfAny
  Option(11).filter(predicateIntLess10).printIfAny
 }

object list extends App {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */

  sealed trait List[+T] {

    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */
    def ::[A >: T](elem: A): List[A] = new ::(elem, this)

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */
    def mkString(delim: String = ","): String = this match {
      case Nil => ""
      case ::(head, Nil) => s"$head"
      case ::(head, tail) => s"$head$delim${tail.mkString(delim)}"
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[T] = {
      @tailrec
      def stackMove(a: List[T], b: List[T]):List[T] = a match {
        case Nil => b
        case ::(head, tail) => stackMove(tail, new ::(head, b))
      }
      stackMove(this, Nil)
    }

    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[F](f: T => F): List[F] = {
      @tailrec
      def stackMoveF(a: List[T], b: List[F]): List[F] = a match {
        case Nil => b
        case ::(head, tail) => stackMoveF(tail, new ::(f(head), b))
      }
      stackMoveF(this, Nil).reverse
    }

    def join[A >: T](toTail:List[A]): List[A] = {
      def appendHead[T](a: List[T], b: List[T]):List[T] = a match {
        case Nil => b
        case ::(head, tail) => appendHead(tail, new ::(head, b))
      }
      appendHead(this.reverse,toTail)
    }

    /**
     * flatMap
     */
    def flatMap[F](f: T => List[F]): List[F] = {
      @tailrec
      def stackMoveF(a: List[T], b: List[F]): List[F] = a match {
        case Nil => b
        case ::(head, tail) => stackMoveF(tail, f(head).reverse.join(b))
      }
      stackMoveF(this, Nil).reverse
    }

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(f: T => Boolean): List[T] = {
      @tailrec
      def func(a: List[T], b: List[T]): List[T] = a match {
        case Nil => b
        case ::(head, tail) =>
          if (f(head)) func(tail, new ::(head, b))
          else func(tail, b)
      }
      func(this,Nil).reverse
    }
  }

  case class ::[A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {
    /**
     * Конструктор, позволяющий создать список из N - го числа аргументов
     * Для этого можно воспользоваться *
     *
     * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
     * def printArgs(args: Int*) = args.foreach(println(_))
     */
    def apply[T](v: T*): List[T] = if (v.isEmpty) Nil else new ::(v.head, apply(v.tail: _*))

  }

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(lst: List[Int]): List[Int] = lst.map(_ + 1)

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(lst: List[String]): List[String] = lst.map(s => "!" + s)


  val listA = 1 :: List(2,3,4)
  val listB = 1 ::Nil
  val listS = List("first","second","third")
  println(listA)
  println (listA.mkString())
  println (listA.reverse.mkString())
  println(listA.map(_*10).mkString())

  println(listA.join(listB).mkString())
  println(listA.flatMap(x=>List(x,x*10,x*100)).mkString())
  println(listA.filter(_%2==0).mkString())
  println(incList(listA).mkString())
  println(shoutString(listS).mkString(" && "))

}

/**
 * ::
apply
mkString
reverse
map
flatMap
filter
incList
shoutString
 */