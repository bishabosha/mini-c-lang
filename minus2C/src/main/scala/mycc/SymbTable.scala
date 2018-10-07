package mycc

import scala.collection.mutable

object SymbTable {
  private val map: mutable.Map[Any, Any] = new mutable.HashMap()

  def put(key: Any, value: Any): Unit = map.put(key = key, value = value)
  def get(key: Any): Any = map.get(key = key).orNull
  override def toString: String = map.toString()
}
