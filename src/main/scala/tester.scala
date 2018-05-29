import org.scalameter. _

object tester {


class GdayThread extends Thread {
  override def run(): Unit = {
    println("good day")
    println("simple thread example")
  }
}


  def startThreads = {
    val t1 = new GdayThread
    val t2 = new GdayThread
    t1.start
    t2.start
    t1.join
    t2.join
  }

  def main(args: Array[String]): Unit = {
    val result = measure {
      (0 until 10000000).toArray
    }

    println(result)
  }

}
