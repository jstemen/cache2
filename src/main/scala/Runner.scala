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
  val cache = new CacheImpl[Int, String](3, source)

  //Array(0,1,2,3,3,3,1,1,4,5,0).foreach { i =>
  Array(0, 1, 2, 1, 1, 4, 5, 0).foreach { i =>
    println(s"$i => ${cache.get(i)}")
    println("=" * 20)
  }

}

