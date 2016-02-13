package models

import scala.language.implicitConversions

object MyClass extends Enumeration {

  type MyClass = Value
  val class1, class2, class3 = Value

  def classInTag(x: MyClass): String = x.toString

}

