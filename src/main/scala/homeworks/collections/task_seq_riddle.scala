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
    currentLine.foldRight(List[(Int, Int)]()) {
      (i, acc) =>
        acc match {
          case Nil => List((i, 1))
          case (ii, n) :: xs if ii == i => (ii, n + 1) :: xs
          case _ => (i, 1) :: acc
        }
    }.flatMap { case (i, n) => List(n, i) }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(nextLine)


}