package scala.com.blogspot.jaredstemen

import scala.collection.mutable

trait Repository[KeyType, ValueType] {
  def get(key: KeyType): Option[ValueType]
}


class Node[KeyType](private var _backwards: Option[Node[KeyType]], private var _forwards: Option[Node[KeyType]], val key: KeyType) {

  //Take care of bi-directional linking
  backwards = _backwards
  forwards = _forwards


  def backwards = _backwards

  def backwards_=(n: Option[Node[KeyType]]) {
    //unlink from old
    _backwards match {
      case Some(node) => node._forwards = None
      case None =>
    }

    //link to new
    _backwards = n
    n match {
      case Some(va) => va._forwards = Option(this)
      case None =>
    }
  }

  def forwards = _forwards

  def forwards_=(n: Option[Node[KeyType]]) {

    //unlink from old
    _forwards match {
      case Some(node) => node._backwards = None
      case None =>
    }

    //link to new
    _forwards = n
    n match {
      case Some(va) => va._backwards = Option(this)
      case None =>
    }
  }

  override def equals(o: scala.Any): Boolean = {
    o match {
      case other: Node[KeyType] =>
        key == other.key
      case _ =>
        key == o
    }
  }

  override def hashCode(): Int = {
    key.hashCode()
  }

  override def toString(): String = {
    s"key: ${key}"
  }

  def fullToString(): String = {
    var loc = this

    val sb: scala.StringBuilder = new StringBuilder("head    ")
    while (loc.backwards.isDefined) {
      sb.append(s"<key: ${loc.key}>  -> ")
      loc = loc.backwards.get
    }
    sb.append(s"<key: ${loc.key}>  -> ")
    sb.append("    tail")

    sb.toString
  }

}

class CacheImpl[KeyType, ValueType](val maxSize: Int, val source: Repository[KeyType, ValueType]) extends Repository[KeyType, ValueType] {

  val keyNodeKeymap: mutable.Map[KeyType, Node[KeyType]] = mutable.Map[KeyType, Node[KeyType]]()
  val map: mutable.Map[Node[KeyType], ValueType] = mutable.Map[Node[KeyType], ValueType]()
  var headOpt: Option[Node[KeyType]] = None

  override def toString: String = {
    s"""
      keyNodeKeymap: ${keyNodeKeymap.toString}
      map: ${map}
      headOpt: ${}
    """.stripMargin

  }

  def reprioritizeNodeKey(nodeKey: Node[KeyType]): Unit = {
    //remove nodeKey from queue
    nodeKey.backwards match {
      case Some(back) => {
        nodeKey.forwards match {
          case Some(foward) =>
            println("pinch out node")
            foward.backwards = Option(back)
          //back.forwards = Option(foward)
          case None =>
            println("We are on the head node")
            headOpt = Option(back)
        }
      }
      case None => {
        nodeKey.forwards match {
          case Some(foward) =>
            println("We are on the last node")
            foward.backwards = None
          case None =>
            println("We are the ONLY node")
            headOpt = None
        }
      }
    }

    val genNode = Option(new Node(headOpt, None, nodeKey.key))

    //Add node to front of queue
    headOpt = genNode
  }

  def removeNodeToMakeSpace() = {
    headOpt match {
      case Some(head) =>
        //TODO should be possible to track the tail by updating it each time we remove nodes.
        var last = head
        while (last.backwards.isDefined) {
          last = last.backwards.get
        }
        last.forwards.get.backwards = None
        keyNodeKeymap.remove(last.key)
        map.remove(last)

      case None =>
        throw new RuntimeException("Called remove node on an empty data set!")
    }


  }

  def addToCache(nodeKey: Node[KeyType], v: ValueType) {
    keyNodeKeymap(nodeKey.key) = nodeKey
    map += nodeKey -> v
    nodeKey.backwards = headOpt
    headOpt = Option(nodeKey)
  }

  override def get(key: KeyType): Option[ValueType] = {

    println(s"get value for ${key} ")
    val nodeKeyOpt: Option[Node[KeyType]] = keyNodeKeymap.get(key)
    val ret = nodeKeyOpt match {
      case Some(nodeKey) =>
        println("*cache HIT")
        reprioritizeNodeKey(nodeKey)
        val cachedValueOpt = map.get(nodeKey)
        cachedValueOpt
      case None =>
        println("*cache MISS")
        val v = source.get(key)
        v match {
          case Some(value) =>
            if (map.size == maxSize) {
              removeNodeToMakeSpace()
            }
            addToCache(new Node(None, None, key), value)
          case None => // Don't do anything
        }
        v
    }
    println(s"keyNodeKeymap is ${keyNodeKeymap} ")
    println(s"map is ${map} ")
    headOpt match {
      case Some(head) =>
        println(s"head is ${head.fullToString()} ")
      case None =>
        println(s"head is ${headOpt} ")
    }
    ret
  }


}


