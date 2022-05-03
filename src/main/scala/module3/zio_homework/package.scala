package module3

import com.typesafe.config.Config
import module3.zio_homework.RunningTimeService.Service
import module3.zio_homework.config.AppConfig
import pureconfig.ConfigObjectSource
import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource.default
import pureconfig.error.ConfigReaderFailures
import zio.clock.Clock
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._
import zio.{ExitCode, Has, IO, Task, UIO, ULayer, URIO, ZIO, ZLayer, clock}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val lineToInt: URIO[String, Option[Int]] = ZIO.fromFunction[String, Option[Int]](str => str.toIntOption)

  val intro = ZIO.effectTotal(println("Please enter your variant (1-3):"))

  def attempt: ZIO[Console, IOException, Option[Int]] = lineToInt compose getStrLn

  val lotteryDraw: URIO[Random, Option[Int]] = nextIntBetween(1, 4).asSome

  def lotteryResult: ZIO[Random with Console, IOException, Boolean] = attempt.zipWith(lotteryDraw)(_ == _)

  def banner(isWin: Boolean): URIO[Console, Unit] = putStrLn(if (isWin) "You win!" else "You lose!")

  lazy val guessProgram = for {
    _ <- intro
    win <- lotteryResult
    _ <- banner(win)
  } yield (win)

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhileNot[R, E, A](zio: ZIO[R, E, A])(predicate: A => Boolean): ZIO[R, E, A] = for {
    a <- zio
    r <- if (!predicate(a)) doWhileNot(zio)(predicate) else ZIO.succeed(a)
  } yield r

  def doWhileNot2[R, E, A](zio: ZIO[R, E, A])(predicate: A => Boolean): ZIO[R, E, A] = zio.flatMap(a =>
    if (!predicate(a)) doWhileNot(zio)(predicate) else ZIO.succeed(a))

  val guessProgramWhileNotWin =  doWhileNot(guessProgram)(w => w) <*> putStrLn("The End!")

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  import pureconfig.ConfigSource
  import pureconfig._
  import pureconfig.generic.auto._

  val defaultsSource: ConfigObjectSource = ConfigSource.string("{ app-name = John, app-url = 33 }")

  lazy val defaultConfig: Task[AppConfig] = Task.effect(defaultsSource.loadOrThrow[AppConfig])

  def loadConfig: Task[AppConfig] = config.load

  val loadConfigOrDefault: ZIO[Any, Nothing, AppConfig] =
    loadConfig
      .orElse(defaultConfig)
      .orElse(ZIO.succeed(AppConfig("defaultAppName", "defaultAppUrl")))

  val loadConfigAndPrint = for {
    cfg <- loadConfigOrDefault
    _ <- putStrLn(cfg.toString)
  } yield cfg


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = ZIO.sleep(1 seconds) *>  nextIntBetween(0, 11)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Console with Random with Clock, Nothing, List[Int]] =  for {
    list <- ZIO.collectAll(effects)
    _ <- putStrLn(list.toString())
  } yield (list)

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: ZIO[Console with Random with Clock, Nothing, List[Int]] = for {
    list <- ZIO.collectAllPar(effects)
    _ <- putStrLn(list.toString())
  } yield (list)

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */
  type RunningTimeService = Has[RunningTimeService.Service]

  @accessible
  object RunningTimeService {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    class ServiceImpl extends Service {
      val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)

      override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for {
        start <- currentTime
        z <- zio
        finish <- currentTime
        _ <- putStrLn(s"Running time: ${finish - start}")
      } yield z
    }

    val live: ULayer[RunningTimeService] = ZLayer.succeed(new RunningTimeService.ServiceImpl)
  }

  /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[Console with Clock with Random with RunningTimeService, Nothing, List[Int]] = for {
    runningTimeService <- ZIO.environment[RunningTimeService].map(_.get)
    listTimes <- runningTimeService.printEffectRunningTime(app)
  } yield listTimes

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */
  lazy val appRT: ZIO[Console with Clock with Random, Nothing, List[Int]] =
    appWithTimeLogg.provideSomeLayer[Console with Clock with Random](RunningTimeService.live)

}
