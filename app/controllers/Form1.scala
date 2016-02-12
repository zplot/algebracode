package controllers

import models.Blackboard
import models.EntryForm

import play.api._
import play.api.mvc._
import play.api.data.{ Form }
import play.api.data.Forms._


object Form1 extends Controller {

  val tmp = {

    val title: String = "Test"
    val title5fields: String = "Test"
    val label1: String = "Please review"
    val subject: String = "Test"
    val project: String = "Test"
    val references: String = "Test"
    val text1: String = "Test"
    val text2: String = "Test"

    EntryForm.EntryForm1(
      title,
      title5fields,
      label1,
      subject,
      project,
      references,
      text1,
      text2
    )


  }



  def createForm1fields() = Action {

    val title: String = "Test"
    val title5fields: String = "Test"
    val label1: String = "Test"
    val subject: String = "Test"
    val project: String = "Test"
    val references: String = "Test"
    val text1: String = "Test"
    val text2: String = "Test"

    val par = EntryForm.EntryForm1(
                              title,
                              title5fields,
                              label1,
                              subject,
                              project,
                              references,
                              text1,
                              text2
                              )

    Ok(views.html.form1(par))
  }

  val input1fields = Form(mapping(

    "input1" -> nonEmptyText)(EntryForm.EntryFields1.apply)(EntryForm.EntryFields1.unapply))



  def createone() = Action { implicit request =>
    input1fields.bindFromRequest.fold(
      //formWithErrors => Forbidden("Invalid submission!"),
      formWithErrors => Ok(views.html.form1(tmp)),

      value => Ok(views.html.blackboard(Blackboard.Blackboard("a","b","c","d","e",List[(String,String)](("p", models.algebra.FiniteGroupExamples.S(3).cayleyTable2)),"h","i","j","k","l"))))
  }

  def process5(value: EntryForm.EntryForm): String = {

    value.text1 + " * " + value.text2
  }


}

