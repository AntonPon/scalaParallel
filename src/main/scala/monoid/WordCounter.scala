package monoid
import main.scala.parallel._

import scala.io.Source
object WordCounter {

  //type TripleSuper = (String, Int, String)

  sealed trait SuperCounter {
    override def toString: String = {
      this match {
        case CharElement(s) => s"${s}"
        case CurrentCounter(l, n, r) => s"left word $l, word number $n, right word $r"

      }
    }
  }

  case class CharElement(symbol: String) extends SuperCounter
  case class CurrentCounter(left: String, wordNumber: Int, right: String) extends  SuperCounter


  trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
  }

  def foldMapSegment[A, B](xs: IndexedSeq[A], from: Int, to: Int, m: Monoid[B])(f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to){
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }

  def foldMapPar[A, B](xs: IndexedSeq[A],
                       from: Int, to: Int,
                       m: Monoid[B])
                      (f: A=> B)
                      (implicit threshold: Int): B = {
    if (to - from < threshold) foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from  + (to - from)/2
      val (l, r) = parallel(foldMapPar(xs, from, middle, m)(f)(threshold),
        foldMapPar(xs, middle, to, m)(f)(threshold))
      m.op(l, r)
    }
  }


  def main(args: Array[String]): Unit = {
    var ar = IndexedSeq("")

    val bufferedSource = Source.fromFile("src/main/file.txt")
    for (line <- bufferedSource.getLines()){
      ar = ar ++ IndexedSeq(line, "")

    }
    bufferedSource.close

    val monoid = new  Monoid[(String, Int, String)] {
      override def op(x: (String, Int, String), y: (String, Int, String)): (String, Int, String) = {
         if (x._3 == "" && y._1 == "") (x._1, x._2 + y._2, y._3)
         else  (x._1, x._2 + y._2 + 1, y._3)
      }

      override def zero: (String, Int, String) = ("", 0, "")
    }

    def f(line: String): (String, Int, String) = {
      if (line == "") ("", 0, "")
      else{
        val stringArray = line.split(" +")
        (stringArray(0), stringArray.length-2, stringArray(stringArray.length-1))
      }
    }

    val res = foldMapPar(ar, 0, ar.length, monoid)(f)(3)
    println(s" first word [${res._1}] words number [${res._2}] last word [${res._3}]")

    object NonEmptySpaceString {
      def unapply(s: String) = if (s.forall(_.isSpaceChar) && s.length > 0) Some(s) else None
    }

    val monoid2 = new  Monoid[SuperCounter] {
      override def op(x: SuperCounter, y: SuperCounter): SuperCounter =
        (x, y) match {
/*          case(CharElement(c1), CharElement(c2)) =>{
            if (c1 == " ") CurrentCounter(c2, 0, c1)
              else if (c2 == " ") CurrentCounter(c1, 0, c2)
            else {
              CharElement(c1+c2)
            }
          }
          case (CharElement(c1), CurrentCounter(l, n, r)) => {
            if (l == " ")
          }*/

          case (CharElement(leftStub), CharElement(rightStub)) =>
            CharElement(leftStub + rightStub)

          case (CharElement(leftStub), CurrentCounter("", wordCount, rightStub)) =>
            CurrentCounter(leftStub, wordCount, rightStub)

          case (CurrentCounter(leftStub, wordCount, ""), CharElement(rightStub)) =>
            CurrentCounter(leftStub, wordCount, rightStub)

          case (CharElement(NonEmptySpaceString(_)), CurrentCounter(_, wordCount, rightStub)) =>
            CurrentCounter("", wordCount + 1, rightStub)

          case (CurrentCounter(leftStub, wordCount, _), CharElement(NonEmptySpaceString(_))) =>
            CurrentCounter(leftStub, wordCount + 1, "")

          case (CharElement(leftLeftStub), CurrentCounter(leftRightStub, wordCount, rightStub)) =>
            CurrentCounter(leftLeftStub + leftRightStub, wordCount, rightStub)

          case (CurrentCounter(leftStub, wordCount, rightLeftStub), CharElement(rightRightStub)) =>
            CurrentCounter(leftStub, wordCount, rightLeftStub + rightRightStub)

          case (CurrentCounter(leftStub, leftWordCount, centreLeftStub), CurrentCounter(centreRightStub, rightWordCount, rightStub)) =>
            CurrentCounter(
              leftStub,
              leftWordCount + rightWordCount + (if ((centreLeftStub + centreRightStub) == "") 0 else 1),
              rightStub
            )

      }

      override def zero: SuperCounter = new CharElement("")
    }

    def f2(line: String): SuperCounter = {
      CharElement(line)
    }




    var ar2 = IndexedSeq[String]()

    val bufferedSource2 = Source.fromFile("src/main/file.txt")
    for (line <- bufferedSource2.getLines()){
      var index = 0
      while (index < line.length){
        ar2 = ar2 ++ IndexedSeq[String](s"${line.charAt(index)}")
        index = index +1
      }



    }

    bufferedSource2.close

    val res2 = foldMapPar(ar2, 0, ar2.length, monoid2)(f2)(300)

    println(res)
  }


}
