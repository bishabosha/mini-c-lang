package mycc
package exception

case class UnimplementedError(msg: String) extends Exception(msg)