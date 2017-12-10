package com.blogspot.jaredstemen

import org.mockito.Mockito._
import org.mockito.Matchers._

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.com.blogspot.jaredstemen.{CacheImpl, Repository}

class MySpec extends WordSpec with MockitoSugar with Matchers {

  "A cache" when {
    "it has a max size of zero" should {
      "throw an exception" in {

        val source: Repository[String, String] = mock[Repository[String, String]]
        intercept[IllegalArgumentException] {
          new CacheImpl[String, String](0, source)
        }
      }
    }

    "caching 2 items" should {
      "discard oldest data" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(Option("1"))
        when(source.get("two")).thenReturn(Option("2"))
        val cache = new CacheImpl[String, String](2, source)
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

      "cache missing items" in {
        val source: Repository[String, String] = mock[Repository[String, String]]
        when(source.get("one")).thenReturn(None)
        val cache = new CacheImpl[String, String](3, source)
        cache.get("one") shouldBe None
        cache.get("one") shouldBe None
        verify(source).get("one")
        verifyNoMoreInteractions(source)
      }

    }

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
        when(source.get("one")).thenReturn(Option("1"))
        when(source.get("two")).thenReturn(Option("2"))
        val cache = new CacheImpl[String, String](1, source)
        cache.get("one") shouldBe Option("1")
        cache.get("two") shouldBe Option("2")
        cache.get("two") shouldBe Option("2")
        cache.get("one") shouldBe Option("1")
        verify(source, times(2)).get("one")
        verify(source).get("two")
        verifyNoMoreInteractions(source)
      }
    }

  }

}