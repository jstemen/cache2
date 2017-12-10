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

    var looped = false
    val sb: scala.StringBuilder = new StringBuilder("head    ")
    while (loc.backwards.isDefined && !looped) {
      sb.append(s"<key: ${loc.key}>  -> ")

      if (loc.backwards.get.forwards.get != loc) {
        throw new RuntimeException(s"loc: ${loc} loc.backwards is ${loc.backwards} and backward foward is ${loc.backwards.get.forwards}")
      }
      loc = loc.backwards.get
      looped = loc == this
    }
    //sb.append(s"<key: ${loc.key}>  -> ")
    sb.append("    tail")

    sb.toString
  }

}

class CacheImpl[KeyType, ValueType](val maxSize: Int, val source: Repository[KeyType, ValueType]) extends Repository[KeyType, ValueType] {

  val keyNodeKeymap: mutable.Map[KeyType, Node[KeyType]] = mutable.Map[KeyType, Node[KeyType]]()
  val map: mutable.Map[Node[KeyType], Option[ValueType]] = mutable.Map[Node[KeyType], Option[ValueType]]()
  var headOpt: Option[Node[KeyType]] = None

  override def toString: String = {
    s"""
      keyNodeKeymap: ${keyNodeKeymap.toString}
      map: ${map}
      headOpt: ${}
    """.stripMargin

  }

  def reprioritizeNodeKey(nodeKey: Node[KeyType]): Unit = {
    //Don't reprioirtize if we are the head already
    if (nodeKey == headOpt.get) {
      return
    }

    val newHead = new Node(headOpt, None, nodeKey.key)
    val origHead = headOpt
    val origTail = lastOpt
    //remove nodeKey from queue
    nodeKey.backwards match {
      case Some(back) => {
        nodeKey.forwards match {
          case Some(foward) =>
            println("pinch out node")
            foward.backwards = Option(back)
            origHead.get.forwards = Option(newHead)
            newHead.forwards = origTail
            headOpt = Option(newHead)
          //back.forwards = Option(foward)
          case None =>
            println("We are on the head node")
            throw new IllegalStateException(s"Forward node is undefined for $nodeKey")
        }
      }
      case None => {
        throw new IllegalStateException(s"Backward node is undefined for $nodeKey")
      }
    }

  }

  def lastOpt: Option[Node[KeyType]] = {
    headOpt match {
      case Some(head) =>
        head.backwards
      case None => None
    }
  }

  def removeNodeToMakeSpace() = {
    println("removing node....")
    lastOpt match {
      case Some(last) =>
        last.forwards match {
          case Some(secondToLast) =>
            secondToLast.backwards = None
          case None =>
            //head == last
            headOpt = None
        }
        keyNodeKeymap.remove(last.key)
        map.remove(last)
      case None =>
        throw new RuntimeException("Called remove node on an empty data set!")
    }
  }

  def addToCache(nodeKey: Node[KeyType], v: Option[ValueType]) {
    keyNodeKeymap(nodeKey.key) = nodeKey
    map += nodeKey -> v
    headOpt match {
      case Some(_) =>
        val tail = lastOpt
        nodeKey.backwards = headOpt
        nodeKey.forwards = tail
      case None =>
        //We are the only node, link to self
        nodeKey.backwards = Option(nodeKey)
    }
    headOpt = Option(nodeKey)
  }

  override def get(key: KeyType): Option[ValueType] = {

    println(s"******** get value for ${key} *************")
    val nodeKeyOpt: Option[Node[KeyType]] = keyNodeKeymap.get(key)
    val ret: Option[ValueType] = nodeKeyOpt match {
      case Some(nodeKey) =>
        println("*cache HIT")
        reprioritizeNodeKey(nodeKey)
        val cachedValueOpt: Option[ValueType] = map(nodeKey)
        cachedValueOpt
      case None =>
        println("*cache MISS")
        val v = source.get(key)
        if (map.size == maxSize) {
          removeNodeToMakeSpace()
        }
        addToCache(new Node(None, None, key), v)
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


