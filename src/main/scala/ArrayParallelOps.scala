import org.scalameter.{Key, Warmer, config}
import main.scala.parallel._


import scala.util.Random

object ArrayParallelOps {

  def power(x: Int, p: Double): Double = math.exp(p * math.log(math.abs(x))).toInt

  def mapArraySegment[A, B](
    source: Array[A],
    from: Int,
    to: Int,
    op: A => B,
    target: Array[B]
  ) = {
    var index = from
    while(index < to){
      target(index) = op(source(index))
      index = index +1
    }
  }

  val threshold = 1000

  def mapArraySegmentPar[A, B]
  (
    source: Array[A],
    from: Int, to: Int,
    op: A=> B,
    target: Array[B]) : Unit = {
    if (to - from < threshold) mapArraySegment(source, from, to, op, target)
    else {
      val middle = from + (to - from)/2
      parallel(mapArraySegmentPar(source, from, middle, op, target), mapArraySegmentPar(source, middle, to, op, target))
    }
  }


  def raiseArrayToPower
  (
  source: Array[Int],
  from: Int, to: Int,
  p: Double,
  target: Array[Double]
  ): Unit = {
    if (to - from < threshold) {
      var index = from
      while(index < to){
        target(index) = power(source(index), p)
        index = index + 1
       }
    }else {
      val middle =from + (to - from)/2
      parallel(raiseArrayToPower(source, from ,middle, p, target), raiseArrayToPower(source, middle, to, p, target))
    }
  }


  def main(args: Array[String]): Unit = {
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 280,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false).withWarmer(new Warmer.Default)

    val rnd = new Random
    val length = 1000000
    val source = (0 until length).map(_ * rnd.nextInt).toArray
    val target = Array.fill(length){0.0}
    val p = 1.3
    val f: Int => Double = x => power(x, p)

    println("Sequential computation ...")
    val seqtime = standardConfig.measure {
      mapArraySegment(source,0, length, f, target)
    }

    println(s"complete sequential time $seqtime")

    println("Parallel computation ...")
    val partime = standardConfig measure {
      mapArraySegmentPar(source, 0, length, f, target)
    }

    println(s"complete parallel time $partime")

    println("specialised parallel time")
    val partimeSpec = standardConfig.measure {
      raiseArrayToPower(source, 0, length, p, target)
    }

    println(s" completed in $partimeSpec\n\n")

    println(s"speedup seq map vs par map ${seqtime.value / partime.value}")
    println(s"speedup par map vs spec par map ${partime.value / partimeSpec.value}")
  }

}
