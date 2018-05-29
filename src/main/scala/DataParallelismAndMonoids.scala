import main.scala.parallel._
object DataParallelismAndMonoids {

  def fold[A](list: List[A])
             (zero: A)
             (op: (A, A) => A):A = {
    if (list.isEmpty) zero
    else op(list.head, fold(list.tail)(zero)(op))
  }



  def scan[A](list: List[A])
             (zero: A)
             (op: (A, A) => A): List[A]= {
    def scan0(input: List[A], resultList: List[A], resultVal: A): List[A] = {
      if (input.isEmpty) resultList
      else {
        val newResultVal = op(input.head, resultVal)
        scan0(input.tail, newResultVal :: resultList, newResultVal)
      }
    }
    scan0(list, List(zero), zero)
  }

  def mapArraySegment[A, B](source: Array[A],
                            from: Int,
                            to: Int,
                            f: A => B,
                            target: Array[B]): Unit = {
    var index = from
    while (index < to) {
      target(index) = f(source(index))
      index = index + 1
    }
  }


  val threshold = 1000

  def mapArraySegmentPar[A, B](
                            source: Array[A],
                            from: Int,
                            to: Int,
                            f: A => B,
                            target: Array[B]): Unit = {
    if (to - from < threshold) mapArraySegment(source, from, to, f, target)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(mapArraySegmentPar(source, from, middle, f, target),
        mapArraySegmentPar(source, middle, middle, f, target))
    }
  }

/*
  def mapTreePar[A, B](
    source: Tree[A],
    f: A => B): Tree[B]= {

  }
*/





  sealed trait Tree[T]
  case class Leaf[T](block: Array[T])
  case class Node[T] (left: Tree[T], right: Tree[T])

  def main(args: Array[String]): Unit = {

    println(fold(List(1, 2, 3))(42)((x, acc) => x + acc))
    println(scan(List(1, 2, 3))(42)((x, acc) => x + acc))
  }

}
