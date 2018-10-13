package object mycc {

  /**replace these to match expected values at each context level in C.y
   */
  def specialize(ast: CAst): Ast = ParseCAst.specialize(ast)
}