package module3


import zio.clock.{Clock, nanoTime}
import zio.console.{Console, getStrLn, putStrLn}

import java.io.IOException
import scala.concurrent.Future
import scala.io.{Source, StdIn}
import scala.util.Try
import zio.duration._
import scala.language.postfixOps
import zio.Task
import zio.IO
import zio.RIO
import zio.URIO
import zio.UIO
import zio.ZIO



/** **
 * ZIO[-R, +E, +A] ----> R => Either[E, A]
 *
 */

// val f: String => Either[Throwable, Int] = ???

// f()

object toyModel {


  /**
   * Используя executable encoding реализуем свой zio
   */
   
   case class ZIO[-R, +E, +A](run: R => Either[E, A]){ self =>

      def map[B](f: A => B): ZIO[R, E, B] = 
        ZIO(r => self.run(r).map(f))
      
      def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = 
         ZIO( r => self.run(r).fold(ZIO.fail, f).run(r))
   }




  /**
   * Реализуем конструкторы под названием effect и fail
   */
  object ZIO{
    
    def effect[A](a: => A): ZIO[Any, Throwable, A] = try{
      ZIO(_ => Right(a))
    } catch{
      case e: Throwable => fail(e)
    }

    def fail[E](e: E): ZIO[Any, E, Nothing] =  
      ZIO(_ => Left(e))
  }







  /** *
   * Напишите консольное echo приложение с помощью нашего игрушечного ZIO
   */

   lazy val echo: ZIO[Any, Throwable, Unit] = for{
     _ <- ZIO.effect(println("Please enter your name:"))
     str <- ZIO.effect(StdIn.readLine())
     _ <- ZIO.effect(println(str))
   } yield ()

  //execute
  //echo.run()

  type Error
  type Environment

  // ZIO[-R, +E, +A]

  //type T[A] = ZIO[Any, Throwable, A]



  lazy val _: Task[Int] = ??? // ZIO[Any, Throwable, Int]
  lazy val _: IO[Error, Int] = ??? // ZIO[Any, Error, Int]
  lazy val _: RIO[Environment, Int] = ??? // ZIO[Environment, Throwable, Int]
  lazy val _: URIO[Environment, Int] = ??? // ZIO[Environment, Nothing, Int]
  lazy val _: UIO[Int] = ??? // ZIO[Any, Nothing, Int]
}


object zioConstructors {


  // константа
  val z1: UIO[Int] = ZIO.succeed(7)


  // любой эффект
  val z2: Task[Unit] = ZIO.effect(println("Hello"))

  // любой не падающий эффект

  val z3: UIO[Unit] = ZIO.effectTotal(println("Hello"))




  // From Future
  val f: Future[Int] = ???
  val z4: Task[Int] = ZIO.fromFuture(ec => f)


  // From try
  lazy val t: Try[String] = ???
  lazy val z5: Task[String] = ZIO.fromTry(t)



  // From either
  lazy val e: Either[String, Int] = ???
  lazy val z6: IO[String,Int] = ZIO.fromEither(e)




  // From option
  lazy val opt : Option[Int] = ???
  lazy val z7: IO[Option[Nothing], Int] = ZIO.fromOption(opt)

  val z77: URIO[Any,Option[Int]] = z7.option
  val z78: ZIO[Any,Option[Nothing],Int] = z77.some
 val x999: ZIO[Any, Option[Nothing], Option[Int]] = z7.asSome



  // From function
  lazy val z8: URIO[String, Int] = ZIO.fromFunction[String, Int](str => str.toInt)

  // особые версии конструкторов

  lazy val _: UIO[Unit] = ZIO.unit

  lazy val _: UIO[Option[Nothing]] = ZIO.none

  lazy val _: UIO[Nothing] = ZIO.never // while(true)

  lazy val _: ZIO[Any, Nothing, Nothing] = ZIO.die(new Throwable("Ooops"))

  lazy val _: ZIO[Any, Int, Nothing] = ZIO.fail(1)

}



object zioOperators {

  /** *
   *
   * 1. Создать ZIO эффект который будет читать строку из консоли
   */

  lazy val readLine: Task[String] = ZIO.effect(StdIn.readLine())

  /** *
   *
   * 2. Создать ZIO эффект который будет писать строку в консоль
   */

  def writeLine(str: String): UIO[Unit] = ZIO.effectTotal(println(str))

  /** *
   * 3. Создать ZIO эффект котрый будет трансформировать эффект содержащий строку в эффект содержащий Int
   */

  lazy val lineToInt: URIO[String, Int] = ZIO.fromFunction[String, Int](str => str.toInt)
  /** *
   * 3.Создать ZIO эффект, который будет работать как echo для консоли
   *
   */

  lazy val echo = for {
    str <- readLine
    _ <- writeLine(s"echo:$str")
  } yield ()


  /**
   * Создать ZIO эффект, который будет привествовать пользователя и говорить, что он работает как echo
   */

  lazy val greetAndEcho: ZIO[Any, Throwable, Unit] =  for {
    _ <-writeLine("Please enter your name:")
    str <- readLine
    _ <- writeLine(s"Hi,$str")
  } yield()


  // Другие варианты композиции

  lazy val a1: Task[Unit] = ??? // println()
  lazy val b1: Task[String] = ???


  lazy val z9: ZIO[Any,Throwable,(Unit, String)] = a1 zip b1

  lazy val z10: ZIO[Any,Throwable, String] = a1 *> b1

  lazy val z11: ZIO[Any,Throwable, Unit] = a1 <* b1


  // greet and echo улучшенный
  lazy val _: ZIO[Any, Throwable, Unit] = ZIO.effect(println("Hi, I'm echo")).zipLeft(echo)
  lazy val _x: ZIO[Any, Throwable, Unit] = ZIO.effect(println("Hi, I'm echo")).zipRight(echo)
  lazy val _y: ZIO[Any, Throwable, (Unit, Unit)] = ZIO.effect(println("Hi, I'm echo")).zip(echo)


  /**
   * Используя уже созданные эффекты, написать программу, которая будет считывать поочереди считывать две
   * строки из консоли, преобразовывать их в числа, а затем складывать их
   */

  val readMyInt: ZIO[Any, Throwable, Int] = lineToInt compose readLine
  val readPairAndSum: ZIO[Any, Throwable, Int] = readMyInt.zipWith(readMyInt)(_ + _) //r1
  val r1 = readPairAndSum


  /**
   * Второй вариантfor
   */

  val r2: ZIO[Any, Throwable, Int] = for {
    a <- readMyInt
    b <- readMyInt
  } yield a + b



  /**
   * Доработать написанную программу, чтобы она еще печатала результат вычисления в консоль
   */

  lazy val r3 = for {
    x <- readPairAndSum
    _ <- writeLine(s"sum = $x")
  } yield()




  lazy val a: Task[Int] = Task.succeed(55)
  lazy val b: Task[String] = Task.succeed("Happy end!")

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab1: ZIO[Any, Throwable, (Int, String)] = a zip b


  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab2: ZIO[Any, Throwable, Int] = a zipLeft b

  /**
   * последовательная комбинация эффектов a и b
   */
  lazy val ab3: ZIO[Any, Throwable, String] = a zipRight b


  /**
   * Последовательная комбинация эффета b и b, при этом результатом должна быть конкатенация
   * возвращаемых значений
   */
  lazy val ab4: ZIO[Any,Throwable, String] = b.zipWith(b)(_ + _)


  /**
    * 
    * Другой эффект в случае ошибки
    */

    val ab5: ZIO[Any, Throwable, Any] = ab2.orElse(ab3)

  /**
    * 
    * A as B
    */
  val bx: ZIO[Any, Nothing, Int] = Task.succeed("Hi boys!").as(77)

  def readFile(fileName: String): ZIO[Any, IOException, String] =
    ZIO.effect(Source.fromFile(fileName).getLines().take(1).mkString)
      .orElse(ZIO.fail(new IOException("problem!")))

  // из эффекта с ошибкой, в эффект который не падает

  val d: URIO[Any, String] = readFile("").orDie
  
}
