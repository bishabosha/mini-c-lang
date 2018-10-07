package mycc

import scala.collection.mutable

object SymbTable {
  private val map: mutable.Map[String, Any] = new mutable.HashMap()

  def put(key: String, value: Any): Unit = map.put(key = key, value = value)
  def get(key: String): Any = map.get(key = key).orNull
  override def toString: String = map.toString
}
