package futures

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

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
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val list: List[Future[Either[A, Throwable]]] = futures.map {
      f =>
        f.transformWith {
          case Failure(exception) => Future.successful(Right(exception))
          case Success(value) => Future.successful(Left(value))
        }
    }
    Future.sequence(list).map {
      list =>
        (list.collect{case Left(a) => a}, list.collect{case Right(throwable) => throwable})
    }
  }

  /**
   * Алтернативный вариант на случай, если предполагается, что нельзя использовать метод Future.sequence. Читается
   * чуть хуже (субъективно), но тоже работает.
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence2[A](futures: List[Future[A]])
                      (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    futures.foldLeft(Future.successful((List[A](), List[Throwable]()))) {
      (cur, future) =>
        cur.flatMap{
          buffer =>
            future.transformWith {
              case Failure(exception) => Future.successful((buffer._1, buffer._2 :+ exception))
              case Success(value) => Future.successful((buffer._1 :+ value, buffer._2))
            }
        }
    }
  }

}
