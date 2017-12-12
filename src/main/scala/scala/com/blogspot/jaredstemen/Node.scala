package scala.com.blogspot.jaredstemen

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
    val sb: scala.StringBuilder = new StringBuilder()
    while (loc.backwards.isDefined && !looped) {
      sb.append(s"${loc.backwards.get.key} <=  [key: ${loc.key}] => ${loc.forwards.get.key}\n")

      if (loc.backwards.get.forwards.get != loc) {
        throw new RuntimeException(s"loc: ${loc} loc.backwards is ${loc.backwards} and backward foward is ${loc.backwards.get.forwards}")
        //println(s"loc: ${loc.key} loc.backwards is ${loc.backwards.get.key} and backward foward is ${loc.backwards.get.forwards.get.key}")
      }
      loc = loc.backwards.get
      looped = loc == this
    }
    //sb.append(s"<key: ${loc.key}>  -> ")
    sb.append("tail")

    sb.toString
  }

}

