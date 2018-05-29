package parallel_Count

import main.scala.parallel._

import scala.util.Random

object ParalleCountPoints {

  def closest_pair(points: Seq[(Int, Int)]): (Double, Seq[(Int, Int)]) = {

    def getMiddle(xList: Seq[Int]): Int= {
      xList.length / 2
    }

    def mergeByY(seqL: Seq[(Int, Int)], seqR: Seq[(Int, Int)] ): Seq[(Int, Int)] = {
      (seqL ++ seqR).sortBy(_._2)
    }


    def boundaryMerge(seqMerged: Seq[(Int, Int)], deltaL: Double, deltaR: Double, median: Double ): Double = {
      var deltaMin = Math.min(deltaL, deltaR)
      val m = seqMerged.partition(point => point._1 >= median - deltaMin || point._1 <= median + deltaMin)._1
      val numberOfExamples = Math.min(8, m.length - 2)

      def distance(left: (Int, Int), right: (Int, Int)): Double = {
        Math.sqrt(Math.pow(left._1 - right._1, 2) + Math.pow(left._2 - right._2, 2))
      }

   /*   var idx = 0
      while (idx < m.length-numberOfExamples){
        var idx2 = 1
        while(idx+idx2 < m.length){
          deltaMin = Math.min(deltaMin, distance(m(idx), m(idx+idx2)))
          idx2 = idx2 +1
        }
        idx = idx+1
      }

      deltaMin*/


           def getMinDistance(seq: Seq[(Int, Int)], currentIdx: Int, endIdx: Int, minValue: Double): Double = {

              //println(seq.length, currentIdx, endIdx, minValue)


              def getCurrentMin(seq: Seq[(Int, Int)], pointOneIdx: Int, currentPointIdx: Int, maxPointIdx: Int, minPointValue: Double): Double = {
                if (currentPointIdx == maxPointIdx) minPointValue
                else {
                  val pointOne = seq(pointOneIdx)
                  val pointTwo = seq(pointOneIdx + currentPointIdx)
                  val minCurPointValue = Math.min(minPointValue, Math.sqrt(Math.pow(pointOne._1-pointTwo._1, 2) + Math.pow(pointOne._2 - pointTwo._2, 2)))

                  getCurrentMin(seq, pointOneIdx, currentPointIdx+1, maxPointIdx, minCurPointValue)
                }
              }

              if (currentIdx > endIdx) minValue
              else {
                if (seq.length > 8){
                  val maxPointIdx = 8
                  val minCurValue = Math.min(minValue, getCurrentMin(seq, currentIdx, 1, maxPointIdx, minValue))
                  getMinDistance(seq, currentIdx+1, endIdx, minCurValue)
                }else {
                  val maxPointIdx = seq.length - currentIdx
                  val minCurValue = Math.min(minValue, getCurrentMin(seq, currentIdx, 1, maxPointIdx, minValue))
                  getMinDistance(seq, currentIdx+1, endIdx, minCurValue)
                }

              }

            }
      if (m.length > 7) getMinDistance(m, 0, m.length-8, deltaMin)
      else getMinDistance(m, 0, m.length-1, deltaMin)

    }

    if (points.length < 2) (Double.PositiveInfinity, points)
    else {
      val middle = getMiddle(points.map(_._1).sorted)
      //val median = getMedian(points.sortBy(_._1))
      //val sortedPoints = points.sortBy(_._1)
      val (l, r) = points.splitAt(middle) //partition(point => point._1 < median)
      val ((deltaL, seqL), (deltaR, seqR)) = parallel(closest_pair(l), closest_pair(r))

      val seqMerged = mergeByY(seqL, seqR)
      val deltaMerged = boundaryMerge(seqMerged, deltaL, deltaR, points(middle)._1)
      (deltaMerged, seqMerged)
    }

  }

  //@tailrec
  def seqClosestPair(points: Seq[(Int, Int)], startIdx: Int, endIdx: Int, distance: Double): Double = {
    //println(distance)
    //var deltaMin = distance
    def closestPerPoint(seq: Seq[(Int, Int)], startCurIdx: Int, endCurIdx: Int, distanceCur: Double): Double = {
      if (startCurIdx == endCurIdx + 1) distanceCur
      else {
        val pointOne = seq(startIdx)
        val pointTwo = seq(startCurIdx)
        val minDist = Math.min(distanceCur, Math.sqrt(Math.pow(pointOne._1-pointTwo._1, 2) + Math.pow(pointOne._2-pointTwo._2, 2)))
        closestPerPoint(seq, startCurIdx + 1, endCurIdx, minDist)
      }
    }

    if (startIdx == endIdx -1) distance
    else {
      val minTotalDist = Math.min(distance, closestPerPoint(points, startIdx + 1, endIdx, distance))
      seqClosestPair(points, startIdx + 1, endIdx, minTotalDist)
    }
/*
    var idx = 0
    while(idx < points.length-1){
      var idx2 = 1
      while (idx2 + idx < points.length){
        val pointOne = points(idx)
        val pointTwo = points(idx+ idx2)
        //val minCurPointValue = Math.min(minPointValue, Math.sqrt(Math.pow(pointOne._1-pointTwo._1, 2) + Math.pow(pointOne._2 - pointTwo._2, 2)))
        deltaMin = Math.min(deltaMin, Math.sqrt(Math.pow(pointOne._1-pointTwo._1, 2) + Math.pow(pointOne._2 - pointTwo._2, 2)))
        idx2 = idx2 + 1
      }
      idx = idx+1
      if (deltaMin == 0) println(idx)
    }
    deltaMin*/
  }

  def main(args: Array[String]): Unit = {

    val randomX = new Random()
    val randomY = new Random()


    val totalNumberOfPoints = 1600
    val maxPossibleValue = 10000
   val seq = Seq.fill(totalNumberOfPoints)(randomX.nextInt(maxPossibleValue), randomY.nextInt(maxPossibleValue))
    //println(seq.partition(point => point._1 > 300))
    //println(seq.partition(point => point._1 > 300)._1.partition(point => point._1 < 600)._1)
//   val seq = Seq((0, 2), (1, 0), (100, 500), (101, 600), (20, 30), (-80, -130), (0, 100), (-200, -300))
  // val seq = Seq((-242, -42), (-125, -63), (-108, 99), (-93, 117), (-67, -9), (-57, 165), (-44, -43), (-25, -279),
   //   (-14, -86),(28, -150),(73, 14),(90, -14),(100, 3),(126, -8),(149, -6),(220, 21))
    val timeStart = System.currentTimeMillis()
    val (dist, points) = closest_pair(seq)
    println("parallel end ", (System.currentTimeMillis() - timeStart)/1000)
    println(dist)

    val timeStartPar = System.currentTimeMillis()
    val distNotPar = seqClosestPair(seq, 0, seq.length-1, Double.PositiveInfinity)
    println("non parallel end ", (System.currentTimeMillis() - timeStartPar)/1000)
    println(distNotPar)
  }
}
