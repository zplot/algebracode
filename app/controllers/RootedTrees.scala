
// Imports de los árboles
import models.Blackboard.Blackboard
import models.MyHTML._
import models.MyClass._

import scala.language.implicitConversions
import models.botany._
import models.botany._
import models.botany._
import play.api.mvc._




object RootedTrees extends Controller {

  def index = Action {

    Ok(views.html.index("Algebra & Functional Code"))

  }

  /*def printTree3 = Action {

    val title = "Primeros dibujos"
    val arbol: Tree3 = Node3.string2Tree3("***^*^^*^**^**^*^*^*^*^^*^^**^^")

    val draw3: PrintableDraw = Tree.scaleDraw(Tree.string2Draw(arbol.toString))
    val texto = arbol.toString
    val parameters = (title, texto, draw3)
    Ok(views.html.trees(parameters))

  }*/
}
