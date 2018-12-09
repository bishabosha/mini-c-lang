package mycc
package exception

case class UnexpectedAstNode(msg: String) extends Exception(msg)