package module3.zio_homework
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, URIO, ZIO}

object ZioHomeWorkApp extends zio.App {
  override def run(args: List[String]): URIO[Clock with Random with Console, ExitCode] =
    module3.zio_homework.runApp.exitCode
}


object TestRun {
   def main(args: Array[String]): Unit = {
     runAppWithTimeLogg()
  }

  def runGuessProgram(): Unit = {
    zio.Runtime.default.unsafeRun(module3.zio_homework.guessProgram.forever)
  }

  def runDoWhile(): Unit = {
    val zioEffect: URIO[Random with Console, Int] = for {
      console <- ZIO.environment[Console].map(_.get)
      integer <- module3.zio_homework.randomPositiveNonZeroInteger(100)
      _ <- console.putStrLn(integer.toString)
    } yield (integer)

    zio.Runtime.default.unsafeRun(module3.zio_homework.doWhile[Random with Console, Nothing, Int](_ == 1, zioEffect))
  }

  def loadConf(): Unit = {
    val defaultConfig: config.AppConfig  = module3.zio_homework.config.AppConfig("TestName", "TestURL")
    zio.Runtime.default.unsafeRun(module3.zio_homework.loadConfigOrDefault(defaultConfig))
  }

  def task_4_4(): Unit = {
    zio.Runtime.default.unsafeRun(module3.zio_homework.app)
  }

  def task_4_5(): Unit = {
    zio.Runtime.default.unsafeRun(module3.zio_homework.appSpeedUp)
  }

  def runAppWithTimeLogg(): Unit = {
    zio.Runtime.default.unsafeRun(module3.zio_homework.runApp)
  }

}