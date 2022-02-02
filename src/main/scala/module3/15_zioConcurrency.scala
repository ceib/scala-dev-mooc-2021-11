package module3

import zio.{Ref, UIO, URIO, ZIO, clock}
import zio.clock.{Clock, sleep}
import zio.console.{Console, putStr, putStrLn}
import zio.duration.durationInt
import zio.internal.Executor

import java.util.concurrent.TimeUnit
import scala.language.postfixOps
import zio.Task
import zio.Fiber

object zioConcurrency {

  // время старта
  // время финиша
  // метод выполнять

  // эфект содержит в себе текущее время
  val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)




  /**
   * Напишите эффект, который будет считать время выполнения любого эффекта
   */


  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for{
    start <- currentTime
    z <- zio
    finish <- currentTime
    _ <- putStrLn(s"Running time: ${finish - start}")
  } yield z


  val exchangeRates: Map[String, Double] = Map(
    "usd" -> 76.02,
    "eur" -> 91.27
  )

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  val sleep1Second: URIO[Clock, Unit] = ZIO.sleep(1 seconds)

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  val sleep3Seconds: URIO[Clock, Unit] = ZIO.sleep(3 seconds)

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation1 спустя 3 секунды
   */
     lazy val getExchangeRatesLocation1 = sleep3Seconds *> putStrLn("GetExchangeRatesLocation1")

  /**
   * Создать эффект который печатает в консоль GetExchangeRatesLocation2 спустя 1 секунду
   */
    lazy val getExchangeRatesLocation2 = sleep1Second *> putStrLn("GetExchangeRatesLocation2")



  /**
   * Написать эффект котрый получит курсы из обеих локаций
   */
  lazy val getFrom2Locations = getExchangeRatesLocation1 <*> getExchangeRatesLocation2


  /**
   * Написать эффект котрый получит курсы из обеих локаций паралельно
   */
  lazy val getFrom2LocationsInParallel = for{
      fiber <- getExchangeRatesLocation1.fork
      r2 <- getExchangeRatesLocation2
      r1 <- fiber.join
  } yield (r1, r2)


  /**
   * Предположим нам не нужны результаты, мы сохраняем в базу и отправляем почту
   */


   val writeUserToDB = sleep1Second *> putStrLn("User in DB")

   val sendMail = sleep1Second *> putStrLn("Mail sent")

  /**
   * Написать эффект котрый сохранит в базу и отправит почту паралельно
   */


  lazy val writeAndSand = for{
      _ <- writeUserToDB.fork
      _ <- sendMail.fork
      _ <- ZIO.sleep(5 seconds)
  } yield ()


  /**
   *  Greeter
   */

  lazy val greeter = for{
      _ <- (sleep1Second *> putStrLn("Hello")).forever.fork
      _ <- ZIO.sleep(5 seconds)
  } yield ()
  /***
   * Greeter 2
   * 
   * 
   * 
   */

 lazy val hello: ZIO[Console, Nothing, Unit] = putStrLn("Hello") *> hello

 lazy val greeter2 = for{
     f1 <- hello.fork
     _ <- f1.interrupt
     _ <- f1.join
 } yield()
  

  /**
   * Прерывание эффекта
   */

   lazy val app3 = for{
       f1 <- ZIO.effect(while(true) println("GetExchange")).fork
       r2 <- getExchangeRatesLocation2
       _ <- f1.interrupt

   } yield()



  /**
   * Получние информации от сервиса занимает 1 секунду
   */
  def getFromService(ref: Ref[Int]) = for {
    count <- ref.getAndUpdate(_ + 1)
    _ <- putStrLn(s"GetFromService - ${count}") *> ZIO.sleep(1 seconds)
  } yield ()

  /**
   * Отправка в БД занимает в общем 5 секунд
   */
  def sendToDB(ref: Ref[Int]): ZIO[Clock with Console, Exception, Unit] = for {
    count <- ref.getAndUpdate(_ + 1)
    _ <- ZIO.sleep(5 seconds) *> putStrLn(s"SendToDB - ${count}")
  } yield ()


  /**
   * Написать программу, которая конкурентно вызывает выше описанные сервисы
   * и при этом обеспечивает сквозную нумерацию вызовов
   */

  
  lazy val app1 = ???

  /**
   *  Concurrent operators
   */




  lazy val p1 = getExchangeRatesLocation1 zipPar getExchangeRatesLocation2
  lazy val p2 = getExchangeRatesLocation2 race getExchangeRatesLocation1

  lazy val p3 = ZIO.foreachPar(List(1, 2, 3, 4, 5))(el => 
      (sleep1Second  *> putStrLn(el.toString)))






  /**
   * Lock
   */


  // Правило 1
  lazy val doSomething: UIO[Unit] = ???
  lazy val doSomethingElse: UIO[Unit] = ???

  lazy val executor: Executor = ???

  lazy val eff = for{
    f1 <- doSomething.fork
    _ <- doSomethingElse
    r <- f1.join
  } yield r

  lazy val result = eff.lock(executor)



  // Правило 2
  lazy val executor1: Executor = ???
  lazy val executor2: Executor = ???



  lazy val eff2 = for{
      f1 <- doSomething.lock(executor2).fork
      _ <- doSomethingElse
      r <- f1.join
    } yield r

  lazy val result2 = eff2.lock(executor1)  



}