package models

import scala.language.implicitConversions

object MyHTML {

  trait MyHTML

  case class P(cl: String) extends MyHTML
  case class H1(cl: String) extends MyHTML
  case class H2(cl: String) extends MyHTML
  case class H3(cl: String) extends MyHTML
  case class H4(cl: String) extends MyHTML
  case class H5(cl: String) extends MyHTML
  case class H6(cl: String) extends MyHTML

  case class BR(cl: String) extends MyHTML

}

