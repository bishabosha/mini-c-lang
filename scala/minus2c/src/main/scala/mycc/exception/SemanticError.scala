package mycc.exception

case class SemanticError(msg: String) extends Exception(msg)