package models

import scala.language.implicitConversions

object EntryFormC {

  case class EntryFormC(

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

  case class EntryFieldsC(

                          input1: String

                        )


  case class EntryForm1C(

                         title: String,
                         title5fields: String,
                         label1: String,
                         subject: String,
                         project: String,
                         references: String,
                         text1: String,
                         text2: String

                       )

  case class EntryFields1C(input1: String)


}


