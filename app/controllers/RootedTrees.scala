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

    import models.botany.Node3._

    val title = "Drawing rooted trees"

    val arbolito1: Tree3 = "**u**uu"
    val arbolito2: Tree3 = "***uu*u"

    val texto = arbolito1.toString
    val arbolPintable: PrintableDraw = arbolito1.toPrint
    val parameters: (String, String, PrintableDraw) = (title, texto, arbolPintable)

    Ok(views.html.trees3(parameters))

  }

  def drawRootedTrees4 = Action {

    import models.botany.Node4._

    val title = "Drawing rooted trees"

    val arbolito1: Tree4 = "**u**uu"
    val arbolito2: Tree4 = "***uu*u"


    val texto = arbolito2.toString
    val arbolPintable: PrintableDraw4 = arbolito2.canonicalForm.toPrint
    val parameters: (String, String, PrintableDraw4) = (title, texto, arbolPintable)

    Ok(views.html.trees4(parameters))

  }


}
