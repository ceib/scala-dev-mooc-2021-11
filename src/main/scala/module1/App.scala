import scala.util.control.Breaks._
import module3.functional_effects

object App {

  def main(args: Array[String]): Unit = {

      functional_effects.functionalProgram.declarativeEncoding.interpret(functional_effects.functionalProgram.declarativeEncoding.p1)
  }

}
