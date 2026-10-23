package bl.unused

import scala.collection.mutable

/** Remembers the value computed for each key */
final class Memo[K, V] {
  private val values = mutable.HashMap.empty[K, V]

  def apply(key: K)(compute: => V): V = values.synchronized(values.getOrElseUpdate(key, compute))
}
