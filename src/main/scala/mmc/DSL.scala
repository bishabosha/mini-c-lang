package mmc

import mmclib.{_, given}

object DSL:

  object BinaryNode:
    def unapply(node: CAst) =
      node.asBinaryNode(binary.ast.tpe, binary.a1, binary.a2)

  object UnaryNode:
    def unapply(node: CAst) =
      node.asUnaryNode(unary.ast.tpe, unary.a1)

  object Singleton:
    def unapply(node: CAst) =
      node.asSingleton(singleton.ast.tpe)

  object TokenInt:
    def unapply(node: CAst) =
      node.asTokenInt(tokenInt.ast.tpe, tokenInt.value)

  object TokenString:
    def unapply(node: CAst) =
      node.asTokenString(tokenString.ast.tpe, tokenString.lexeme)

  object Sequence:
    def unapply(node: CAst) =
      val tpe = node.ast.tpe
      node.asSequence(tpe, sequence(tpe, binary.a1, binary.a2))

  private def sequence(tpe: String, a1: CAst, a2: CAst): List[CAst] =
    var left    = a1
    var right   = a2
    var list    = List.empty[CAst]
    var reverse = false
    var decided = false
    var break   = false
    while !break do
      val leftinfo = left.ast
      val rightinfo = right.ast
      if leftinfo.tag.isBinary && leftinfo.tpe == tpe then
        if !decided then
          decided = true;
        list = right :: list
        left.binaryOp:
          left  = binary.a1
          right = binary.a2
      else if rightinfo.tag.isBinary && rightinfo.tpe == tpe then
        if !decided then
          decided = true
          reverse = true
        list  = left :: list
        right.binaryOp:
          left  = binary.a1
          right = binary.a2
      else
        if reverse then
          list = right :: left :: list
        else
          list = left :: right :: list
        break = true
    if reverse then list.reverse else list

  def (node: CAst) show: String = printAst0(node, 0, StringBuilder()).toString

  private def printAst0(node: CAst, level: Int, builder: StringBuilder): StringBuilder =
    if node.nonEmpty then
      printLevel(level, builder)
      val ast = node.ast
      node.cata(
        printUnaryNode(unary, level, builder),
        printBinaryNode(binary, level, builder),
        printTokenString(tokenString, builder),
        printTokenInt(tokenInt, builder),
        printSingleton(singleton, builder)
      )
    else
      builder

  private def printUnaryNode(unary: UnaryNodeOps, level: Int, builder: StringBuilder): StringBuilder =
    builder.addAll(s"${unary.ast.tpe}\n")
    printAst0(unary.a1, level + 2, builder)

  private def printBinaryNode(binary: BinaryNodeOps, level: Int, builder: StringBuilder): StringBuilder =
    builder.addAll(s"${binary.ast.tpe}\n")
    printAst0(binary.a1, level + 2, builder)
    printAst0(binary.a2, level + 2, builder)

  private def printTokenString(tokenString: TokenStringOps, builder: StringBuilder): StringBuilder =
    tokenString.ast.tpe match
      case "string" => builder.addAll("" + '"' + tokenString.lexeme + '"' + '\n')
      case _        => builder.addAll(s"${tokenString.lexeme}\n")

  private def printTokenInt(tokenInt: TokenIntOps, builder: StringBuilder): StringBuilder =
    builder.addAll(s"${tokenInt.value}\n")

  private def printSingleton(singleton: SingletonOps, builder: StringBuilder): StringBuilder =
    builder.addAll(s"${singleton.ast.tpe}\n")

  private def printLevel(level: Int, builder: StringBuilder): StringBuilder =
    builder.addAll(" " * level)
