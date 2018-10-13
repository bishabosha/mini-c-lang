package mycc.exception

case class UnimplementedError(msg: String) extends Exception(msg)