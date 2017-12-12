package scala.com.blogspot.jaredstemen

import scala.collection.mutable

trait Repository[KeyType, ValueType] {
  def get(key: KeyType): Option[ValueType]
}





