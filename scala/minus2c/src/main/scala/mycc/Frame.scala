package mycc

import Ast._
import Frame._

object Frame {
  val Empty = Frame(Map(),Map(),Map(),Map())
  type CapturedDefs = Map[(Identifier, Long), Declaration]
  type Defs = Map[Identifier, Declaration]
  type FrameLens = (CapturedDefs => CapturedDefs) => Frame => Frame
  def paramsLens(f: CapturedDefs => CapturedDefs)(frame: Frame): Frame =
    frame.copy(params = f(frame.params))
  def localsLens(f: CapturedDefs => CapturedDefs)(frame: Frame): Frame =
    frame.copy(locals = f(frame.locals))
  def globalsLens(f: Defs => Defs)(frame: Frame): Frame =
    frame.copy(globals = f(frame.globals))
  def capturesLens(f: CapturedDefs => CapturedDefs)(frame: Frame): Frame =
    frame.copy(captures = f(frame.captures))
}

case class Frame(
  globals: Defs,
  locals: CapturedDefs,
  params: CapturedDefs,
  captures: CapturedDefs
)