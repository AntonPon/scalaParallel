package stream

import stream.WorkshopStream.Stream.{cons, empty}

object WorkshopStream {
  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Cons(h, _) => Some(h()) // to read about option and Some
      case Empty => None
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => List()

    }

    def filter(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().filter(p))
      case Cons(_, t) => t().filter(p)
      case _ => Empty

    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }


    def foldRight[B](zero: B)(f: (A, B) => B): B = this match {
      case  Cons(h, t) => f(h(), t().foldRight(zero)(f))
      case _ => zero
    }


    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    def map[B](p: A => B): Stream[B] = this match{
      case Cons(h, t) => cons(p(h()), t().map(p))
      case _ => {val res: Stream[B]  = Empty; res} // ask about empty

    }

    @annotation.tailrec
    private def dropP(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().dropP(n-1)
      case Cons(_, _) if n == 0 => this
      case _ => empty
    }

    def drop(n: Int): Stream[A] =  dropP(n)


    /*def take[A](n: Int): Stream[A] = this match {
      case Cons(hFunc, tFunc) if (n > 0) =>
        Cons(hFunc, () => tFunc().take(n-1))
      case _ => Empty
    }*/
  }

  case object Empty extends  Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h // with val we immediately evaluate right part, but with lazy we delay until head will be reference, and be computed only one time
      lazy val tail = t
      Cons(() => head, () => tail)
    }



    def empty[A]: Stream[A] = Empty

    /*def apply[A](args: A*): Stream[A] = {
      if (args.isEmpty) empty
      else cons(args.head, apply(args.tail: _*))
    }*/

    def apply[A](args: (() => A)*): Stream[A] = {
      if (args.isEmpty) empty
      else cons(args.head(), apply(args.tail: _*))
    }


  }

  def main(args: Array[String]): Unit = {

    val firstStream = Cons(
      () => {println("2+3");2+3},
      () => Cons(
        () => 1 + 7,
        () => Empty
      ))

    println("before stream init")
    println(firstStream.head())
    println(firstStream.head())
    println(firstStream.head())
    println(firstStream.head())
    println("after stream head")

    println("before smart stream init")
    val smartStream = Stream.cons({println("expensive 1"); 1},
      Stream.empty)


    println("before head Option")
    println(smartStream.headOption)
    println(smartStream.headOption)
    println(smartStream.headOption)
    println(smartStream.headOption)
    println(smartStream.headOption)
    println("after head option head")


    println("before args head init")
    val streamOfVariableArgs = Stream(
      () => {println("expensive 1"); 1},
      () => {println("expensive 2"); 2},
      () => {println("expensive 3"); 3},
      () => {println("expensive 4"); 4}
    )
    println("before args head option")
    println(streamOfVariableArgs.headOption)
    println(streamOfVariableArgs.headOption)
    println(streamOfVariableArgs.headOption)
    println(streamOfVariableArgs.headOption)
    println(streamOfVariableArgs.headOption)
    println("after head  args option head")

    println(streamOfVariableArgs.toList)

    println(s"drop(n=2) ${streamOfVariableArgs.drop(2).toList}")
    println(s"forAll ${streamOfVariableArgs.forAll{_ > 2}}")

    val stringStream = Stream(() =>"hello", () => "hello world", () => "hello world hello", () => "hello world hello world")
    println(s"string stream ${stringStream.toList}")
    println(s"from string -> len(string) ${stringStream.map(_.length()).toList}")

    val numberStream = Stream(() => 11, () => 20, () => 30, () => 40, () => 25, () => 26, () => 58)
    println(s"number stream ${numberStream.toList}")
    println(s"filtered number stream ${numberStream.filter(_ % 10 == 0).toList}")

    def prime(int: Int):Boolean = {
      def isPrime(int: Integer, maxValue: Integer, curValue: Integer): Boolean = {
        if (curValue > maxValue) true
        else {
          if ((int % curValue) == 0) false
          else isPrime(int, maxValue, curValue+1)
        }

      }
      if (int < 2) false
      else if ((int >=2) && (int <4) )true
      else isPrime(int, (int/2).toInt, 2)
    }
    println(s"prime filter ${numberStream.filter(prime).toList}")
  }

}
