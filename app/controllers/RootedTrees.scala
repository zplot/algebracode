package controllers

// Imports de los árboles
import controllers.Application._
import models.Blackboard.Blackboard
import models.MyHTML._
import models.MyClass._
import models.Tree
//import models.Tree.{apply => _, _}
import scala.language.implicitConversions
import models.botany._
import play.api.mvc._




object RootedTrees extends Controller {

  def drawRootedTrees = Action {

    val title = "Drawing rooted trees"
    val arbol: String = "***u*u**uu*u*uu*u*u*u**uu*u*u*u*u********uuuuu*uuu*u*uuu*u*uuuu"
    val arbol2: String = "***u*uu**u*uu"
    val arbol3: String = "******u*uuuuu*u"
    val arbol4 = "*********uuuuu*uuuu"
    val arbolTree: Tree3 = Node3.string2Tree3(arbol)
    println("el arbol es = " + arbolTree.nodes)
    val texto = arbol
    val arbolPintable: PrintableDraw = arbolTree.toPrint
    val parameters: (String, String, PrintableDraw) = (title, texto, arbolPintable)

    Ok(views.html.trees3(parameters))

  }
}
