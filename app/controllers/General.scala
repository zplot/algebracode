package controllers

import models.{GeneralEntryForm, BlackboardTable, EntryFormC}
import models.MyHTML._
import models.MyClass._
import models.algebra.FiniteGroup
import models.algebra.FiniteGroupExamples._

import play.api._
import play.api.mvc._
import play.api.data.{ Form }
import play.api.data.Forms._




object General extends Controller {

  val toPresentErrors = {

    val title: String = "Test"
    val title5fields: String = "Test"
    val label1: String = "Please review"
    val subject: String = "Test"
    val project: String = "Test"
    val references: String = "Test"
    val text1: String = "Test"
    val text2: String = "Test"

    GeneralEntryForm.Form1(
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


  // Crea el form para introducir datos
  def createForm() = Action {

    val title: String = "Caley Table of a Permutation Group"
    val title5fields: String = "Caley Table of a Permutation Group"
    val label1: String = "Enter the name of a valid finite group"
    val subject: String = "Permutation Groups"
    val project: String = "Cayley Table"
    val references: String = "These are the references on this subject"
    val text1: String = "What a permutation group is"
    val text2: String = "What a Cayley Table is"

    val par = EntryFormC.EntryForm1C(
      title,
      title5fields,
      label1,
      subject,
      project,
      references,
      text1,
      text2
    )

    Ok(views.html.form1c(par))
  }

  // Aquí está la magia
  val input1fields = Form(mapping(

    "input1" -> nonEmptyText)(GeneralEntryForm.Form2.apply)(GeneralEntryForm.Form2.unapply))

  // Lanza la view que presenta los resultados
  def resultsView() = Action { implicit request =>
    input1fields.bindFromRequest.fold(

      // Hay errores
      formWithErrors => Ok(views.html.errorsview(toPresentErrors)),

      // No hay errores
      value => Ok(views.html.generalblackboard(process(value))))
  }


  def fromGroupStringToCayleyTableOK(s: String): Either[String, List[List[String]]]  = {
    val t = fromStringToGroup(s)
    t match {
      case Right(x)  => Right(x.cayleyTableOK)
      case Left(x) => Left(x)
    }

  }

  def process(value: GeneralEntryForm.Form2): Either[String, List[List[String]]] = {
    fromGroupStringToCayleyTableOK(value.input1)
  }




}






