import org.scalameter.{Key, Warmer, config}
import main.scala.parallel._

import scala.util.Random

object MonteCarloPiEstimation {


  def countPointsInsideCircle(totalNumberOfPoint: Int): Int  = {
    val rndX = new Random()
    val rndY = new Random()

    def simulation(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoint) hits
      else {
        val x = rndX.nextDouble()
        val y = rndY.nextDouble()

        simulation(hits+ (if (x*x + y*y <=1) 1 else 0), pointsGenerated + 1)


      }
    }
    simulation(0, 0)
  }

  def pi(totalNumberOfPoint: Int): Double = 4.0 * countPointsInsideCircle(totalNumberOfPoint)/totalNumberOfPoint

  def piPar(totalNumberOfPoints: Int) = {
    val (pi1, pi2) = parallel(countPointsInsideCircle(totalNumberOfPoints / 2), countPointsInsideCircle(totalNumberOfPoints / 2))
    4 * (pi1 + pi2)/ totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000
    println(pi(totalNumberOfPoints ))

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true).withWarmer(new Warmer.Default)


    val parltime = standartConfig.measure{
      piPar(totalNumberOfPoints)
    }


    val seqtime = standartConfig.measure{
      pi(totalNumberOfPoints)
    }


    println(s"parallel time $parltime")
    println(s"sequential time $seqtime")
    println(s"speedup ${seqtime.value / parltime.value}")
  }
}
