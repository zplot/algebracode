package controllers

// Imports de los árboles
import scala.language.implicitConversions
import models.Tree
import models.Tree._
import play.api.mvc._




object Application extends Controller {

  def index = Action {

    Ok(views.html.index("Algebra & Functional Code"))

  }

  def treeExample = Action {

    val title = "Primeros dibujos"
    val arbol = string2Tree("***^*^^*^**^**^*^*^*^*^^*^^**^^")
    val draw3 = Tree.scaleDraw(Tree.string2Draw(arbol.toString))
    val texto = arbol.toString
    val parameters = (title, texto, draw3)
    Ok(views.html.trees(parameters))

  }

  def blackboard = Action {

    val title = "Blackboard"
    val texto1 = ("p","When \\(a \\ne 0\\), there are two solutions to \\(ax^2 + bx + c = 0\\) and they are")
    val texto2 = ("p", "\\(x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.\\)")
    val texto3 = ("p", "\\[x \\otimes x + 1 \\otimes x\\]")
    val parameters: (String, List[(String, String)]) = (title, List(texto1, texto2, texto3))
    Ok(views.html.inblackboard(parameters))

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

