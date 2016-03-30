package controllers

// Imports de los árboles
import controllers.Application._
import models.Blackboard.Blackboard
import models.MyHTML._
import models.MyClass._
import models.Tree
import models.Tree.{apply => _, _}
import scala.language.implicitConversions
import models.botany._
import play.api.mvc._




object RootedTrees extends Controller {

  def drawRootedTrees = Action {

    val title = "Drawing rooted trees"
    val arbol: String = "**u*u*u*****uuuuu*u"
    val arbol2: Tree3 = Node3.string2Tree3(arbol)
    val texto = arbol
    val arbol3: PrintableDraw = arbol2.toPrint
    val parameters: (String, String, PrintableDraw) = (title, texto, arbol3)
    println(arbol2)
    println(arbol3.edges)
    Ok(views.html.trees3(parameters))

  }
}
