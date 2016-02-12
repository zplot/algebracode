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




  def create() = Action { implicit request =>
    treeForm.bindFromRequest.fold(
      formWithErrors => Forbidden("Invalid submission!"),
      value => Ok(views.html.salidaSimple(("Estamos en la salida", process(value)))))
  }


  def process(value: TwoTrees): String = {

    value.tree1 + " * " + value.tree2
  }


}

