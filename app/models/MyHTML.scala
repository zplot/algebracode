package models

import scala.language.implicitConversions
import MyClass._

object MyHTML extends Enumeration {

  type Tag = Value
  val P, H1, H2, H3, H4, H5, H6, BR = Value

  case class Trio(a: Tag, b: MyClass, c: String)

}

