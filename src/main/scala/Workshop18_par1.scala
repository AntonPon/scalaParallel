import scala.util.Random
import org.scalameter.{Key, Warmer, config}
import main.scala.parallel._

object Workshop18_par1 {


  def main(args: Array[String]): Unit = {
    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true).withWarmer(new Warmer.Default)


    def sumSegment(array: Array[Int], from: Int, to: Int): Int = {
      def iter(sum: Int, index: Int): Int = {
        if(index < to) iter(sum+array(index), index +1)
        else sum
      }
      iter(0, from)

    }

    val random = new Random()
    val array = (0 until 10000000).map(x => random.nextInt(10)).toArray

    val seqtime = standardConfig measure{
      sumSegment(array, 0, array.length)
    }

    val middle = array.length /2
    val partime = standardConfig measure {
      parallel(sumSegment(array, 0,middle), sumSegment(array, middle,array.length))
    }

    println(s"seqtime = $seqtime")
    println(s"partime = $partime")
    val speedup = partime.value / seqtime.value
    println(s"speedup = $speedup")
  }
}
