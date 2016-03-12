package models

import scala.language.implicitConversions

object GeneralEntryForm {


  case class Form1(
                          key: String,
                          title: String,
                          title5fields: String,
                          label1: String,
                          subject: String,
                          project: String,
                          references: String,
                          text1: String,
                          text2: String

                        )

  case class Form2(input1: String)


}


