package com.blogspot.jaredstemen

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.com.blogspot.jaredstemen.{LruCache, Node, Repository}

class NodeSpec extends WordSpec with MockitoSugar with Matchers {

  "A node" when {
    "linked to a new backwards" should {
      val origBack = new Node(None, None, 3)
      val node = new Node(Option(origBack), None, 33)

      /*      "initially be linked to the backwards node" in {
              node.backwards shouldBe (Option(origBack))
            }*/
      val newBack = new Node(None, None, 99)
      node.backwards = Option(newBack)
      "link to the new backwards" in {
        node.backwards.get shouldBe (newBack)
        newBack.forwards.get shouldBe(node)
      }
      "unlink from its old backwards" in {
        origBack.forwards shouldBe (None)
      }


      "linked to a new forwards" should {
        val origforward = new Node(None, None, 3)
        val node = new Node(Option(origforward), None, 33)
        /*        "initially be linked to the forwards node" in {
                  node.forwards shouldBe (Option(origforward))
                }*/
        val newFor = new Node(None, None, 99)
        node.forwards = Option(newFor)
        "link to the new forwards" in {
          node.forwards.get shouldBe (newFor)
          newFor.backwards.get shouldBe (node)
        }
        "unlink from its old forwards" in {
          origforward.backwards shouldBe (None)
        }
      }

    }
  }
}