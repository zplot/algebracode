package controllers

// Imports de los árboles
import models.Blackboard.Blackboard
import models.MyHTML._
import models.MyClass._

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

    val title = "Hello Hopf"
    val notebook = "Hopf algebras"
    val page = "1"
    val next = "next"
    val previous = "previous"
    val t1 = Trio(P,class1,"When \\(a \\ne 0\\), there are two solutions to \\(ax^2 + bx + c = 0\\) and they are")
    val t2 = Trio(P,class1,"\\(x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.\\)")
    val t3 = Trio(P,class1,"\\[x \\otimes x + 1 \\otimes x\\]")
    val content = List(t1, t2, t3)
    val subject = "Hopf algebras"
    val project = "Computational algebra"
    val references = "hopf_references"
    val text1 = "Mauris sit amet ligula est, eget conseact etur lectus maecenas hendrerit suscipit."
    val text2 = "Amet sit lorem ligula est, eget conseact etur lectus hendrerit suscipit maecenas."

    val parameters: Blackboard = Blackboard(
                                            title,
                                            notebook,
                                            page,
                                            next,
                                            previous,
                                            content,
                                            subject,
                                            project,
                                            references,
                                            text1,
                                            text2
                                            )

    Ok(views.html.blackboard(parameters))

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

