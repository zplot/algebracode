package scala

import models.botany._
import models.algebra._
import  Utils._
import scala.language.implicitConversions




object TestTree extends App {

  println("Empezamos Rooted Trees")


  import Node3.string2Tree
  implicit def string2List(s: String): Vector[Node3] = string2Tree(s).children



  val t5 = Node3(Vector())
  println(t5)
  val t6 = Node3(Vector(t5, t5, t5))
  println(t6)
  val t7 = Node3(Vector(t5, t6, t5))
  println("t7 = " + t7)
  println(t7.weight)
  val t8 = Node3.string2Tree(t7.toString)
  println("t8 = " + t8)
  val t9 = "*"
  val test1 = "**^".weight
  println(test1)
  val test2 = Node3("***^^")
  println(test2.weight)







  println("=====================")

  val inicio = Node3("***^*^^*^**^**^*^*^*^*^^*^^**^^")
  val fin = Node3.orderTree(inicio)


  println(inicio)
  println(fin)
  println(inicio == fin)

  val string1 = inicio.toString
  val after = Node3.string2Draw(string1)

  println()
  println("======= string2SVG =======")
  println(inicio)
  println(after)

  println()
  println("======= string2SVG 2 =======")
  val inicio2 = Node3("**^")
  println(inicio2)
  val after2 = Node3.string2Draw(inicio2.toString)
  println(after2)







}




