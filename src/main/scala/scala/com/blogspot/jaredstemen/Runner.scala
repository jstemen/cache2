package scala.com.blogspot.jaredstemen

object Runner extends App {

  val source = new Repository[Int, String] {
    override def get(key: Int) = {
      if (key % 3 == 0) {
        None
      } else {
        Option((key * 10).toString)
      }
    }
  }
  val cache = new LruCache[Int, String](3, source)

  Seq(0, 1, 2, 3, 1, 2, 3, 9, 5, 0).foreach { i =>
    println(s"$i => ${cache.get(i)}")
    println("=" * 20)
  }

}
