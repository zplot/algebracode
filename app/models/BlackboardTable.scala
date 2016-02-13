package models

import scala.language.implicitConversions

object BlackboardTable {

  case class BlackboardTable(

                         title: String,
                         notebook: String,
                         page: String,
                         next: String,
                         previous: String,
                         content: List[List[String]],
                         subject: String,
                         project: String,
                         references: String,
                         text1: String,
                         text2: String

                       )

}


