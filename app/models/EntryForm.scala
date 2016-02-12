package models

import scala.language.implicitConversions

object EntryForm {

  case class EntryForm(

                         title: String,
                         title5fields: String,
                         label1: String,
                         label2: String,
                         label3: String,
                         label4: String,
                         label5: String,
                         subject: String,
                         project: String,
                         references: String,
                         text1: String,
                         text2: String

                       )

  case class EntryFields(


                        input1: String,
                        input2: String,
                        input3: String,
                        input4: String,
                        input5: String


                      )


  case class EntryForm1(

                        title: String,
                        title5fields: String,
                        label1: String,
                        subject: String,
                        project: String,
                        references: String,
                        text1: String,
                        text2: String

                      )

  case class EntryFields1(input1: String)


}


