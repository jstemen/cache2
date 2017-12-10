package com.blogspot.jaredstemen

import org.mockito.Mockito._
import org.mockito.Matchers._

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.com.blogspot.jaredstemen.{CacheImpl, Repository}

class MySpec extends WordSpec with MockitoSugar with Matchers {

  /*  val source = new Repository[Int, String] {
      override def get(key: Int) = {
        if (key % 3 == 0) {
          None
        } else {
          Option((key * 10).toString)
        }
      }
    }*/

  /*  val source: Repository[String, String] = mock[Repository[String, String]]
    when(source.get("one")).thenReturn(Option("1"))
    val cache = new CacheImpl[String, String](3, source)

    //Array(0,1,2,3,3,3,1,1,4,5,0).foreach { i =>
    Seq(0, 1, 2, 1, 1, 4, 5, 0).foreach { i =>
      println(s"$i => ${cache.get(i)}")
      println("=" * 20)
    }*/

  "cache" when {


    "empty" should {
      "return None" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(None)
        val cache = new CacheImpl[String, String](3, source)
        cache.get("one") shouldBe None
        verify(source).get("one")
        verifyNoMoreInteractions(source)
      }
    }
    "not full" should {
      "cache data" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(Option("1")).thenReturn(None)
        val cache = new CacheImpl[String, String](3, source)
        cache.get("one") shouldBe Option("1")
        verify(source).get("one")
        verifyNoMoreInteractions(source)
      }
    }

    "full" should {
      "discard oldest data" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(Option("1")).thenReturn(None)
        when(source.get("two")).thenReturn(Option("2")).thenReturn(None)
        val cache = new CacheImpl[String, String](1, source)
        cache.get("one") shouldBe Option("1")
        cache.get("two") shouldBe Option("2")
        cache.get("two") shouldBe Option("2")
        cache.get("one") shouldBe Option("1")
        verify(source,times(2)).get("one")
        verify(source).get("two")
        verifyNoMoreInteractions(source)
      }
    }

  }

}