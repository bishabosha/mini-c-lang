package mycc.exception

case class UnexpectedAstNode(msg: String) extends Exception(msg)