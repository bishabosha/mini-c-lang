import org.graalvm.polyglot.Value

package object mycc {
  implicit class applyUnit(`this`: Value) {
    def apply(): Unit = `this`.executeVoid()
    def apply[U](value: U): Unit = `this`.executeVoid(value.asInstanceOf[AnyRef])
  }

//  implicit class applyValue(`this`: Value) {
//    def apply(): Value = `this`.execute()
//  }
}
