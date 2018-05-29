import main.scala.parallel._
import org.scalameter._

import scala.util.Random


object pnorms{
  def sumSegment(a: Array[Int], p: Double, from: Int, to: Int): Int = {
    def iter(sum: Int, index: Int): Int = {
      if (index>= to) sum
      else iter(sum + power(a(index), p), index + 1 )
    }
    iter(0, from)
  }

  def power(x: Int, p: Double):Int = {
    math.exp(p * math.log(Math.abs(x))).toInt
  }

  def pnorm(a: Array[Int], p: Double): Int = {
    power(sumSegment(a, p, 0, a.length), 1/p)
  }
  // for this task is not sufficient what exactly value we will take, because it is the minimum valyue after
  // which we will compute sequential array sum
  var threshold = 2

  def sumSegmentPar(a: Array[Int], p: Double, from: Int, to: Int): Int = {

    if (to-from < threshold) sumSegment(a, p, from, to)
    else {
      val middle = from + (to - from)/2
      val (sum1, sum2) = parallel(sumSegmentPar(a, p, from, middle), sumSegmentPar(a, p, middle, to))
      sum1 + sum2
    }
  }

  def pNormPar(a: Array[Int], p: Double): Int = {
    power(sumSegmentPar(a, p, 0, a.length), 1/p)
  }
  def main(args: Array[String]): Unit = {
    val rnd = new Random()
    val length = 1000000
    val input = (0 until length).map(_ * rnd.nextInt()).toArray

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true).withWarmer(new Warmer.Default)


    val parltime = standartConfig.measure{
      pNormPar(input, 2)
    }


    val seqtime = standartConfig.measure{
      pnorm(input, 2)
    }


    println(s"parallel time $parltime ms")
    println(s"sequential time $seqtime ms")
    println(s"speedup ${seqtime.value / parltime.value}")
  }
}
