package module3.zio_homework
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ExitCode, URIO}

object ZioHomeWorkApp extends zio.App {
  // : URIO[Clock with Random with Console, ExitCode]
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
  //zio.Runtime.default.unsafeRun(
  //runApp.orDie
  /**
   * task 1
   */
  //  guessProgram.exitCode

  /**
   * task 2
   */
  //  guessProgramWhileNotWin.exitCode

  /**
   * task 3
   */
  //  loadConfigAndPrint.exitCode

  /**
   * task 4
   */
  //  app.exitCode
  //  appSpeedUp.exitCode
  /**
   * task 5,6
   */
    appRT.exitCode
}
