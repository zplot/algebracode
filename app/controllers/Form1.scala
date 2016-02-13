package controllers

import models.Blackboard
import models.EntryForm
import models.MyHTML._
import models.MyClass._

import play.api._
import play.api.mvc._
import play.api.data.{ Form }
import play.api.data.Forms._


object Form1 extends Controller {

  def paragraph(x: List[String]): List[Trio] = {

    x.map(z => Trio(P, class1, z))

  }

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

      value => Ok(views.html.blackboardmath(Blackboard.Blackboard("a","b","c","d","e", paragraph(process(value).cayleyTableListString),"h","i","j","k","l"))))
  }

  def process(value: EntryForm.EntryFields1): models.algebra.PermutationGroup = value.input1 match {

    case "S(3)" => models.algebra.FiniteGroupExamples.S(3)
    case "S(4)" => models.algebra.FiniteGroupExamples.S(4)
    case "S(5)" => models.algebra.FiniteGroupExamples.S(5)
    case _ => models.algebra.FiniteGroupExamples.Q8


  }


}






















