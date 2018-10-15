package object mycc {

  /**replace these to match expected values at each context level in C.y
   */
  def parseAst(ast: CAst): List[Ast] = ParseCAst.matchTranslationUnit(ast)
}