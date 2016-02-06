package controllers

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
      formWithErrors => Forbidden("Oh noes, invalid submission!"),
      value => Ok("created: " + value))
  }
}

