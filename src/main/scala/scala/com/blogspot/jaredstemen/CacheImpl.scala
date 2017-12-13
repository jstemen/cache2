package scala.com.blogspot.jaredstemen

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CacheImpl[KeyType, ValueType](val maxSize: Int, val source: Repository[KeyType, ValueType]) extends Repository[KeyType, ValueType] {
  if (maxSize < 1) {
    throw new IllegalArgumentException("Catch size must be at least 1")
  }

  private val keyNodeKeymap: mutable.Map[KeyType, Node[KeyType]] = mutable.Map[KeyType, Node[KeyType]]()
  private val map: mutable.Map[Node[KeyType], Option[ValueType]] = mutable.Map[Node[KeyType], Option[ValueType]]()
  private var headOpt: Option[Node[KeyType]] = None


  def cachedContents: Map[KeyType, Option[ValueType]] = {
    var positionOpt = headOpt
    val accumulator = mutable.Map[KeyType, Option[ValueType]]()
    var first = true
    while (positionOpt != headOpt || first) {
      first = false
      val position: Node[KeyType] = positionOpt.get
      accumulator += position.key -> map(position)
      positionOpt = position.backwards
    }

    accumulator.toMap
  }

  override def toString: String = {
    s"""
      keyNodeKeymap: ${keyNodeKeymap.toString}
      map: ${map}
      headOpt: ${}
    """.stripMargin

  }

  private def reprioritizeNodeKey(nodeKey: Node[KeyType]): Unit = {
    //Don't reprioirtize if we are the head already
    if (nodeKey == headOpt.get) {
      return
    }

    unlinkNode(nodeKey)

    //Reinsert node at head
    nodeKey.forwards = lastOpt
    nodeKey.backwards = headOpt
    headOpt = Option(nodeKey)
  }

  private def unlinkNode(nodeKey: Node[KeyType]) = {
    nodeKey.backwards match {
      case Some(back) => {
        nodeKey.forwards match {
          case Some(foward) =>
            println("pinch out node")
            nodeKey.forwards = None
            nodeKey.backwards = None
            foward.backwards = Option(back)


            println("done")
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

  private def lastOpt: Option[Node[KeyType]] = {
    headOpt match {
      case Some(head) =>
        head.forwards
      case None => None
    }
  }

  private def removeNode(node: Node[KeyType]): Unit = {
    println(s"Removing node ${node}")
    unlinkNode(node)

    //Delete reference from other structures
    keyNodeKeymap.remove(node.key)
    map.remove(node)
  }

  private def addToCache(nodeKey: Node[KeyType], v: Option[ValueType]) {
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

        println(s"head is\n ${headOpt.get.fullToString()} ")
        reprioritizeNodeKey(nodeKey)
        val cachedValueOpt: Option[ValueType] = map(nodeKey)
        cachedValueOpt
      case None =>
        println("*cache MISS")
        val v = source.get(key)
        if (map.size == maxSize) {
          removeNode(lastOpt.get)
        }
        addToCache(new Node(None, None, key), v)
        v
    }
    println(s"keyNodeKeymap is ${keyNodeKeymap} ")
    println(s"map is ${map} ")
    headOpt match {
      case Some(head) =>
        println(s"head is \n${head.fullToString()} ")
      case None =>
        println(s"head is $headOpt ")
    }
    ret
  }


}

