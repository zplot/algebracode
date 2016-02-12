package controllers

import models.Blackboard
import models.EntryForm
import models.TwoTrees._
import play.api._
import play.api.mvc._
import play.api.data.{ Form }
import play.api.data.Forms._


object ManualForm extends Controller {

  def createForm() = Action {
    Ok(views.html.manualForm.showForm("Hola caracolas"))
  }

  val treeForm = Form(mapping(

    "tree1" -> nonEmptyText,
    "tree2" -> nonEmptyText)(TwoTrees.apply)(TwoTrees.unapply))



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

    "tree1" -> nonEmptyText,
    "tree2" -> nonEmptyText)(TwoTrees.apply)(TwoTrees.unapply))




/*  def create() = Action { implicit request =>
    treeForm.bindFromRequest.fold(
      formWithErrors => Forbidden("Invalid submission!"),
      value => Ok("created: " + process(value)))
  }*/

  def create() = Action { implicit request =>
    treeForm.bindFromRequest.fold(
      formWithErrors => Forbidden("Invalid submission!"),
      value => Ok(views.html.salidaSimple(("Estamos en la salida", process(value)))))
  }

  def create5fields() = Action { implicit request =>
    treeForm.bindFromRequest.fold(
      formWithErrors => Forbidden("Invalid submission!"),
      value => Ok(views.html.blackboard(Blackboard.Blackboard("","","","","",List[(String, String)](),"","","","",""))))
  }

  def process(value: TwoTrees): String = {

    value.tree1 + " * " + value.tree2
  }


}

