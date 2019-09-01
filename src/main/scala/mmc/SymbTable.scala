package mmc

import scala.collection.mutable

class SymbTable {
  private val map: mutable.AnyRefMap[String, Identifier] = mutable.AnyRefMap()
  def put(id: String): Unit = map.put(key = id, value = Identifier(id))
  def get(id: String): Identifier = map.get(key = id).orNull
  def toMap: Map[String, Identifier] = Map() ++ map
  def clear(): Unit = map.clear
  override def toString: String = map.toString
}
