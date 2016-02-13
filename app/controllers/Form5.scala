package controllers

import models.Blackboard
import models.EntryForm
import models.MyHTML._
import models.MyClass._

import play.api._
import play.api.mvc._
import play.api.data.{ Form }
import play.api.data.Forms._


object Form5 extends Controller {

  val tmp = {

    val title: String = "Test"
    val title5fields: String = "Test"
    val label1: String = "Please review"
    val label2: String = "Please review"
    val label3: String = "Please review"
    val label4: String = "Please review"
    val label5: String = "Please review"
    val subject: String = "Test"
    val project: String = "Test"
    val references: String = "Test"
    val text1: String = "Test"
    val text2: String = "Test"

    EntryForm.EntryForm(
      title,
      title5fields,
      label1,
      label2,
      label3,
      label4,
      label5,
      subject,
      project,
      references,
      text1,
      text2
    )


  }



  def createForm5fields() = Action {

    val title: String = "Test"
    val title5fields: String = "Test"
    val label1: String = "Test"
    val label2: String = "Test"
    val label3: String = "Test"
    val label4: String = "Test"
    val label5: String = "Test"
    val subject: String = "Test"
    val project: String = "Test"
    val references: String = "Test"
    val text1: String = "Test"
    val text2: String = "Test"

    val par = EntryForm.EntryForm(
                              title,
                              title5fields,
                              label1,
                              label2,
                              label3,
                              label4,
                              label5,
                              subject,
                              project,
                              references,
                              text1,
                              text2
                              )

    Ok(views.html.form(par))
  }

  val treeForm5fields = Form(mapping(


    "input1" -> nonEmptyText,
    "input2" -> nonEmptyText,
    "input3" -> nonEmptyText,
    "input4" -> nonEmptyText,
    "input5" -> nonEmptyText)(EntryForm.EntryFields.apply)(EntryForm.EntryFields.unapply))



  def create5fields() = Action { implicit request =>
    treeForm5fields.bindFromRequest.fold(
      //formWithErrors => Forbidden("Invalid submission!"),
      formWithErrors => Ok(views.html.form(tmp)),

      value => Ok(views.html.blackboard(Blackboard.Blackboard("a","b","c","d","e",List[Trio](Trio(P,class1,"g")),"h","i","j","k","l"))))
  }

  def process5(value: EntryForm.EntryForm): String = {

    value.text1 + " * " + value.text2
  }


}

