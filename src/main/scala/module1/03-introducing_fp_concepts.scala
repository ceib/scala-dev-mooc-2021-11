package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



/**
 * referential transparency
 */
 object referential_transparency{


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

object recursion {

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
  def fib(n: Int): Int = {
    @tailrec
    def loop(si: Int, si_1: Int, n: Int): Int = {
      if (n == 1) si
      else loop(si + si_1, si, n-1)
    }
    if (n == 0) 0 else loop(1, 0, n)
  }


}

object hof{

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


 object opt {

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

      /** Реализовать метод printIfAny, который будет печатать значение, если оно есть */
       def printIfAny(): Unit = this match {
           case Option.Some(v) => println(v)
           case Option.None =>
       }

      /** Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option */
       def zip[B](anotherOption: Option[B]): Option[(T, B)] = {
         if (isEmpty || anotherOption.isEmpty) Option.None else Option.Some(this.get, anotherOption.get)
       }

      /** Реализовать метод filter, который будет возвращать не пустой Option в случае если исходный не пуст и предикат от значения = true */
       def filter(f: T => Boolean): Option[T] = this match {
         case Option.Some(v) => if (f(v)) this else Option.None
         case Option.None => Option.None
       }
   }

   object Option{
        case class Some[T](v: T) extends Option[T]
        case object None extends Option[Nothing]

        def apply[T](v: T): Option[T] = if (v != null) Some(v) else None
   }

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T] {

     import List._

     /** Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::` */
     def ::[A >: T](newHead: A): List[A] = this match {
       case list: ::[T] => List.::(newHead, list)
       case Nil => List.::(newHead, Nil)
     }

     /** Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::` */
     def cons[A >: T](newHead: A): List[A] = newHead :: this

     /** Метод mkString возвращает строковое представление списка, с учетом переданного разделителя */
     def mkString(delimiter: String): String = this match {
       case ::(head, tail) =>
         @tailrec
         def loop(list: List[T], string: String): String = list match {
           case ::(head_, tail_) => loop(tail_, string + delimiter + head_.toString)
           case Nil => string
         }
         loop(tail, head.toString)
       case Nil => EmptyString
     }

     /** Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный */
     def reverse: List[T] = this match {
       case l: ::[T] =>
         @tailrec
         def loop(list : List[T], newList: List[T]): List[T] = list match {
           case ::(head, tail) => loop(tail, head :: newList)
           case Nil => newList
         }
         loop(this, Nil)
       case Nil => Nil
     }

     /** Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
       *
       * <p>Реализовано с помощью хвостовой рекурсии, но подобный подход требует вызова .reverse на результирующем списке для сохранения последовательности элементов исходного списка.
       * С точки зрения алгоритмизации лучше использовать формирование списка в цикле с использованием локальных var. Также можно использовать рекурсию не хвостовую, но
       * в этом случае ограничиваем возможные размеры обрабатываемого списка (из-за конечности стека)</p>
       * */
     def map[A](f: T => A): List[A] = this match {
       case list: ::[T] =>
         @tailrec
         def loop(listT: List[T], listA: List[A]): List[A] = listT match {
           case ::(head, tail) => loop(tail, f(head) :: listA)
           case Nil => listA
         }
         loop(list, Nil).reverse
       case Nil => Nil
     }

     /** Реализовать метод filter для списка который будет фильтровать список по некому условию
       *
       * <p>Реализовано с помощью хвостовой рекурсии, но подобный подход требует вызова .reverse на результирующем списке для сохранения последовательности элементов исходного списка.
       * С точки зрения алгоритмизации лучше использовать формирование списка в цикле с использованием локальных var. Также можно использовать рекурсию не хвостовую, но
       * в этом случае ограничиваем возможные размеры обрабатываемого списка (из-за конечности стека)</p>
       */
     def filter(f: T => Boolean): List[T] = this match {
       case list: ::[T] =>
         @tailrec
         def loop(listT: List[T], newList: List[T]): List[T] = listT match {
           case ::(head, tail) =>
             if (f(head)) loop(tail, head :: newList) else loop(tail, newList)
           case List.Nil => newList
         }
         loop(this, Nil).reverse
       case Nil => Nil
     }

   }

   object List {

     final val EmptyString: String = ""

     case class ::[A](head: A, tail: List[A]) extends List[A]
     case object Nil extends List[Nothing]

     /**
       * Конструктор, позволяющий создать список из N - го числа аргументов
       * Для этого можно воспользоваться *
       *
       * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
       * def printArgs(args: Int*) = args.foreach(println(_))
       */
     def apply[A](elements: A*): List[A] = {
       @tailrec
       def loop(list: List[A], seq: scala.collection.Seq[A]): List[A] = {
         if (seq.isEmpty) list else loop(seq.head :: list, seq.tail)
       }
       if (elements.nonEmpty) {
         loop(Nil, elements).reverse
       } else Nil
     }

   }

   /** Написать функцию incList котрая будет принимать список Int и возвращать список,
     * где каждый элемент будет увеличен на 1
     */
   def incList(list: List[Int]): List[Int] = list.map(_ + 1)

   /** Написать функцию shoutString котрая будет принимать список String и возвращать список,
     * где к каждому элементу будет добавлен префикс в виде '!'
     */
   def shoutString(listOfStrings: List[String]): List[String] = listOfStrings.map(str => if (str != null) "!" + str else "!" )

 }