package mycc

import scala.collection.mutable

class SymbTable {
  private val map: mutable.AnyRefMap[String, Any] = mutable.AnyRefMap()
  def put(key: String, value: Any): Unit = map.put(key = key, value = value)
  def get(key: String): Any = map.get(key = key).orNull
  def pack(): Unit = map.repack()
  override def toString: String = map.toString
}
