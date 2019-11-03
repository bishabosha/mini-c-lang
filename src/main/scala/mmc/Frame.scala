package mmc

import Ast._
import Frame._

object Frame
  type CapturedDefs = Map[Scoped, Declaration]
  type Defs         = Map[Identifier, Declaration]
  type FrameLens    = (CapturedDefs => CapturedDefs) => Frame => Frame
  val empty = Frame(Map.empty, Map.empty, Map.empty, Map.empty)
  def paramsLens(f: CapturedDefs => CapturedDefs)(frame: Frame): Frame =
    frame.copy(params = f(frame.params))
  def localsLens(f: CapturedDefs => CapturedDefs)(frame: Frame): Frame =
    frame.copy(locals = f(frame.locals))
  def globalsLens(f: Defs => Defs)(frame: Frame): Frame =
    frame.copy(globals = f(frame.globals))
  def capturesLens(f: CapturedDefs => CapturedDefs)(frame: Frame): Frame =
    frame.copy(captures = f(frame.captures))

case class Frame(
  globals: Defs,
  locals: CapturedDefs,
  params: CapturedDefs,
  captures: CapturedDefs
)
