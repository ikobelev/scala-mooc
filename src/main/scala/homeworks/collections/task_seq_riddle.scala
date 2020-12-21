package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine.foldLeft(List[(Int, Int)]()) {
      (acc, i) =>
        acc match {
          case Nil => List((i, 1))
          case (ii, n) :: xs if ii == i => (ii, n + 1) :: xs
          case _ => (i, 1) :: acc
        }
    }.reverse.flatMap(x => List(x._2, x._1))

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = {
    def loop(l: List[Int]): LazyList[List[Int]] = l #:: loop(nextLine(l))
    loop(List(1))
  }
}