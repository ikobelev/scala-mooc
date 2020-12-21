package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.Nil
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] = {
    val fs = futures.map(f => f.recover { case ex => ex })
    Future.sequence(fs)
      .map(l => l.foldLeft((Nil: List[A], Nil: List[Throwable])) {
        (acc, r) =>
          r match {
            case r: Throwable => (acc._1, r :: acc._2)
            case r: A => (r :: acc._1, acc._2)
          }
      })
      .map(t => (t._1.reverse, t._2.reverse))
  }
}
