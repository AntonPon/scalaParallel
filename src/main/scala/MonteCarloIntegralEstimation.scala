import scala.util.Random
import org.scalameter._
import main.scala.parallel._



object MonteCarloIntegralEstimation {


def countIntegrals(numberOfPoints: Int, function: Double=> Double, a: Double, b: Double): Double= {

  val randomX = new Random()

  def iterateSum(iteration: Int, sum: Double): Double = {
    if (iteration >= numberOfPoints) sum
    else iterateSum(iteration+1, sum + (b-a)*function(a + randomX.nextDouble()*(b-a)))
  }
  iterateSum(0, 0)
}

  def integralPar(numberOfPoints: Int, function: Double => Double, a: Double, b: Double): Double = {
  val (sum1, sum2) = parallel(countIntegrals(numberOfPoints/2, function, a, b), countIntegrals(numberOfPoints/2, function, a, b))
  (sum1+sum2)/numberOfPoints
}

  def integral(numberOfPoints: Int, function: Double => Double, a: Double, b: Double): Double = {
    countIntegrals(numberOfPoints, function, a, b)/numberOfPoints
  }



  def main(args: Array[String]): Unit = {


    val totalNumberOfPoints = 10000000
    val a = 0
    val b = 4
    val function = (x:Double) => x*x

    println("integral par: " + integralPar(totalNumberOfPoints, function, a,b))
    println("integral: " + integral(totalNumberOfPoints, function, a,b))

    val standartConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true).withWarmer(new Warmer.Default)


    val parltime = standartConfig.measure{
      integralPar(totalNumberOfPoints, function, a,b)
    }


    val seqtime = standartConfig.measure{
      integral(totalNumberOfPoints, function, a, b)
    }


    println(s"parallel time $parltime")
    println(s"sequential time $seqtime")
    println(s"speedup ${seqtime.value / parltime.value}")

  }
}
