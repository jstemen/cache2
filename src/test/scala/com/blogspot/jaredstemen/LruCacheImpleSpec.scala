package com.blogspot.jaredstemen

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable
import scala.com.blogspot.jaredstemen.{LruCache, Repository}

class LruCacheImpleSpec extends WordSpec with MockitoSugar with Matchers {

  "A cache" when {
    "it has a max size of zero" should {
      "throw an exception" in {

        val source: Repository[String, String] = mock[Repository[String, String]]
        intercept[IllegalArgumentException] {
          new LruCache[String, String](0, source)
        }
      }
    }

    "caching 2 items" should {
      "discard oldest data" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(Option("1"))
        when(source.get("two")).thenReturn(Option("2"))
        val cache = new LruCache[String, String](2, source)
        cache.get("one") shouldBe Option("1")
        cache.get("two") shouldBe Option("2")
        cache.get("two") shouldBe Option("2")
        cache.get("one") shouldBe Option("1")
        verify(source, times(1)).get("one")
        verify(source).get("two")
        verifyNoMoreInteractions(source)
      }
    }

    "caching" should {
      "not crash" in {

        println("start carsshshsh***********")
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
        //  0
        //  0, 1
        //  0, 1, 2
        //     0, 2, 1
        //        0, 2, 1
        //           2, 1, 4
        //              1, 4, 5
        //                 4, 5, 0
        //                    4, 0, 5
        val ints = Array(0, 1, 2, 1, 1, 4, 5, 0, 5, 3, 2, 1, 3, 4, 4, 4, 4, 4, 2, 4, 2, 1, 1, 0, 0, 2, 2, 4, 4, 2)
        ints.zipWithIndex.foreach { case (k, i) =>
          println(s"i is ${i}")

          val uniLookBack = mutable.HashSet[Int]()
          var j = i
          while (j > -1 && uniLookBack.size < 3) {
            val foundKey = ints(j)
            uniLookBack += foundKey
            j -= 1
          }
          cache.get(k)


          println(s"uniLookback is ${uniLookBack}")
          cache.cachedContents.keys shouldBe uniLookBack
        }
      }

      "cache missing items" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(None)
        val cache = new LruCache[String, String](3, source)
        cache.get("one") shouldBe None
        cache.get("one") shouldBe None
        verify(source, times(1)).get("one")
        verifyNoMoreInteractions(source)
      }

    }

    "empty" should {
      "return None" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(None)
        val cache = new LruCache[String, String](3, source)
        cache.get("one") shouldBe None
        verify(source).get("one")
        verifyNoMoreInteractions(source)
      }
    }
    "not full" should {
      "cache data" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(Option("1")).thenReturn(None)
        val cache = new LruCache[String, String](3, source)
        cache.get("one") shouldBe Option("1")
        verify(source).get("one")
        verifyNoMoreInteractions(source)
      }
    }

    "full" should {
      "discard oldest data" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(Option("1"))
        when(source.get("two")).thenReturn(Option("2"))
        val cache = new LruCache[String, String](1, source)
        cache.get("one") shouldBe Option("1")
        cache.get("two") shouldBe Option("2")
        cache.get("two") shouldBe Option("2")
        cache.get("one") shouldBe Option("1")
        verify(source, times(2)).get("one")
        verify(source, times(1)).get("two")
        verifyNoMoreInteractions(source)
      }
    }

  }

}