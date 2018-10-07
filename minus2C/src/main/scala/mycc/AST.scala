package mycc

trait AST {
  def `type`: Int
}

trait NODE extends AST {
  def left: AST
  def right: AST
}

trait TOKEN extends AST {
  def lexeme: String
  def value: Int
}