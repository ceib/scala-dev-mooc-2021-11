package module3

import module3.zio_homework.config.AppConfig
import zio.{Has, RIO, Task, ULayer, URIO, ZIO, ZLayer}
import zio.clock.Clock
import zio.console._
import zio.duration.durationInt
import zio.random._

import scala.language.postfixOps
import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import zio.macros.accessible

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def intToString(string: String): Task[Int] = ZIO.effect(string.toInt)

  lazy val inputInt: RIO[Console, Int] = for {
    console <- ZIO.environment[Console].map(_.get)
    str <- console.getStrLn
    int <- intToString(str)
  } yield (int)

  lazy val validatedInputInt: URIO[Console, Option[Int]] = inputInt
    .tapError(e => putStrLn( "Неверный ввод. Ошибка: " + e.getMessage))
    .foldM(
      failure = _ => ZIO.none,
      success = int => ZIO.some(int)
    )

  def randomPositiveNonZeroInteger(n: Int): URIO[Random, Int] = for {
    random <- ZIO.environment[Random].map(_.get)
    randomInt <- random.nextIntBounded(n)
  } yield(randomInt + 1)

  lazy val guessProgram: ZIO[Console with Random, Throwable, Unit] = for {
    console <- ZIO.environment[Console].map(_.get)
    _ <- console.putStrLn("Угадай число от 1 до 3:")
    randomInt <- randomPositiveNonZeroInteger(3)
    userInt <- validatedInputInt
    answer <- ZIO.effect( userInt.map(int => if (int == randomInt) "Верно" else "Не угадал").getOrElse("Попробуйте еще раз") )
    _ <- console.putStrLn(answer)
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](conditionF: A => Boolean, zio: ZIO[R, E, A]): ZIO[R, Any, A] = {
    for {
      a <- zio
      isComplete <- ZIO.effect(conditionF(a))
      _ <- if (isComplete) ZIO.succeed(a) else doWhile(conditionF, zio)
    } yield (a)
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault(defaultConfig: AppConfig): URIO[Console, AppConfig] = {
    zio_homework.config.load
      .foldM(
        failure = _ => ZIO.succeed(defaultConfig),
        success = conf => ZIO.succeed(conf)
      ).tap(conf => putStrLn(conf.toString))
  }


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  // Лучше реализовывать как randomPositiveNonZeroInteger(10).delay(1 seconds), но оставлю с zipRight в учебных целях
  lazy val eff: URIO[Random with Clock, Int] = ZIO.sleep(1 seconds) *> randomPositiveNonZeroInteger(10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[URIO[Random with Clock, Int]] = for(_ <- 1 to 10) yield eff

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: URIO[Console with Clock with Random, Int] = printEffectRunningTime(ZIO.collectAll(effects).map(_.sum)).tap(int => putStrLn(int.toString))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: URIO[Console with Clock with Random, Int] = printEffectRunningTime(ZIO.collectAllPar(effects).map(_.sum)).tap(int => putStrLn(int.toString))


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type EffectRunningTimeService = Has[EffectRunningTimeService.Service]

  @accessible
  object EffectRunningTimeService {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    // Класс здесь не обязателен, можно просто ниже в live сделать анонимную реализацию - это будет лаконичнее.
    // Но, субъективно, пока что кажется, что разделяя реализацию сервиса и создание слоя, получаем более легко читаемый код.
    private[EffectRunningTimeService] class LiveServiceImpl extends Service {
      override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] = for{
        start <- currentTime
        z <- zio
        finish <- currentTime
        _ <- putStrLn(s"Running time: ${finish - start}")
      } yield z
    }

    val live: ULayer[EffectRunningTimeService] = ZLayer.succeed(new LiveServiceImpl)

  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[EffectRunningTimeService with Console with Clock with Random, Throwable, Int] = EffectRunningTimeService.printEffectRunningTime(app)

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runAppEnv: ULayer[EffectRunningTimeService] = EffectRunningTimeService.live
  lazy val runApp: ZIO[Random with Console with Clock, Throwable, Int] = appWithTimeLogg.provideSomeLayer[Random with Console with Clock](runAppEnv)
  
}
