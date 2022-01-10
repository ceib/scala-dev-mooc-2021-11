package futures

import HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.tools.nsc.Reporting.WarningCategory.Feature
import scala.util.{Failure, Success, Try}

object task_futures_sequence extends App {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */


  def fullSequence[A](futures: List[Future[A]])(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val acc = Future.successful((List.empty[A], List.empty[Throwable]))
    futures.foldLeft(acc) { (flist, future) =>
      flist.flatMap { case (alist, elist) =>
        future.map(success => (alist :+ success, elist)).recover {
          case error: Throwable => (alist, elist :+ error)
        }
      }
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  val task1 = Future{
    println(s"start  ${Thread.currentThread().getName}");
    Thread.sleep(500)
    println(s"finish ${Thread.currentThread().getName}")
    Thread.currentThread().getName
  }
  val task2 = Future{
    println(s"start2  ${Thread.currentThread().getName}");
    Thread.sleep(500)
    println(s"finish2 ${Thread.currentThread().getName}")
    Thread.currentThread().getName
  }
  Thread.sleep(2000)
  println("go for")
  val x = for {
    t1 <- task1
    t2 <- task1
  } yield t1+t2
  println("end for")
  Thread.sleep(300)

  println(x.value)
}
