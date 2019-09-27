package mmc

import scala.collection.mutable

class SymbTable
  private val set = mutable.HashSet.empty[Identifier]
  def put(id: Identifier): Unit = set += id
  def get(id: Identifier): Boolean = set(id)
  def toSet: Set[Identifier] = set.toSet
  def clear(): Unit = set.clear
  override def toString: String = set.toString
