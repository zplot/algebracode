package models

import scala.language.implicitConversions

object Blackboard {

  case class Blackboard(

                         title: String,
                         notebook: String,
                         page: String,
                         next: String,
                         previous: String,
                         content: List[(String, String)],
                         subject: String,
                         project: String,
                         references: String,
                         text1: String,
                         text2: String

                       )

}


