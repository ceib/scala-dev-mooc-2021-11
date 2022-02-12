import scala.util.control.Breaks._
import module3.functional_effects
import module3.zioRecursion
import module3.multipleErrors
import zio.ExitCode
import zio.URIO
import zio.ZEnv
import zio.ZIO
import module3.zioConcurrency
import module3.di

object App {

  def main(args: Array[String]): Unit = {
    zio.Runtime.default.unsafeRun(???)
  }

}

object ZioApp extends zio.App{
  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    zioConcurrency.printEffectRunningTime(zioConcurrency.app3).exitCode
  }
}