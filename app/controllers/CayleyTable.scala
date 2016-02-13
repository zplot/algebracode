package controllers

import models.Cayley._
import models.EntryForm
import models.MyHTML._
import models.MyClass._

import play.api._
import play.api.mvc._
import play.api.data.{ Form }
import play.api.data.Forms._


object CayleyTable extends Controller {

  def theTable(x: List[String]): List[List[String]] = {

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

      value => Ok(views.html.cayleytable(CayleyTable(theTable(process(value).cayleyTableOK)))))
  }

  def process(value: EntryForm.EntryFields1): models.algebra.PermutationGroup = value.input1 match {

    case "S(3)" => models.algebra.FiniteGroupExamples.S(3)
    case "S(4)" => models.algebra.FiniteGroupExamples.S(4)
    case "S(5)" => models.algebra.FiniteGroupExamples.S(5)
    case _ => models.algebra.FiniteGroupExamples.Q8


  }


}




