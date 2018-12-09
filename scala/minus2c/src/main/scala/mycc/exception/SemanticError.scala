package mycc
package exception

case class SemanticError(msg: String) extends Exception(msg)