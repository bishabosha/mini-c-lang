package mycc

trait Stage {
  type Source
  type Context
  type Goal
  def apply(source: Source): (Context, Goal)
}