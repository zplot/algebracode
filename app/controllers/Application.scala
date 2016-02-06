package controllers

// Imports de los árboles
import scala.language.implicitConversions
import models.Tree
import models.Tree._
import play.api.mvc._




object Application extends Controller {

  def index = Action {

    val title = "Primeros dibujos"
    val arbol = string2Tree("***^*^^*^**^**^*^*^*^*^^*^^**^^")
    val draw3 = Tree.scaleDraw(Tree.string2Draw(arbol.toString))
    val texto = arbol.toString
    val parameters = (title, texto, draw3)
    Ok(views.html.index(parameters))

  }

  def static = Action {

    Ok(views.html.static())

  }



  def salida = Action {

    val title = "Primeros dibujos"
    val arbol = string2Tree("***^*^^*^**^**^*^*^*^*^^*^^**^^")
    val draw3 = Tree.scaleDraw(Tree.string2Draw(arbol.toString))
    val texto = "Salida" + arbol.toString
    val parameters = (title, texto, draw3)
    Ok(views.html.salida(parameters))

  }






}

