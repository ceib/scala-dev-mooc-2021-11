package module3

import zio.random.Random
import zio.test.Assertion.{anything, equalTo}
import zio.test.TestAspect.nonFlaky
import zio.test.environment.TestConsole
import zio.test.{Assertion, DefaultRunnableSpec, Gen, Sized, Spec, TestFailure, TestSuccess, ZSpec, assert, assertM, check, checkM, suite, testM}
import zio_homework._

object ZioHomeworkSpec extends DefaultRunnableSpec{

  val intGen: Gen[Random, Int] = Gen.anyInt

  override def spec: Spec[_root_.zio.test.environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("guessProgram Spec")(
    testM("intToString Test"){
      checkM(intGen) {
        int =>
          assertM(intToString(int.toString))(equalTo(int))
      }
    } @@ nonFlaky(10),
    testM("inputInt Test"){
      for {
        _ <- TestConsole.feedLines("1")
        int <- inputInt
      } yield{
        assert(int)(equalTo(1))
      }
    }
  )

}
