package scala


object TestTreeLayout extends App {

  import models.botany._



  println("Empezamos")

/*  val leaf1 = Node3(Vector[Node3]())
  val leaf2 = Node3(Vector[Node3]())
  val leaf3 = Node3(Vector[Node3]())
  val leaf4 = Node3(Vector[Node3]())
  println(leaf1 == leaf2)
  val node1 = Node3(Vector(leaf1, leaf2, leaf3, leaf4))
  val tree1: Tree3 = Tree3(node1)

  println(tree1)
  TreeLayaut.layaut(tree1)
  println(leaf1.x)
  println(leaf1.y)
  println(leaf2.x)
  println(leaf2.y)
  println(leaf3.x)
  println(leaf3.y)
  println(leaf4.x)
  println(leaf4.y)
  println(node1.x)
  println(node1.y)

  println()
  println("Capitulo 1")
  println()*/
  val arbol: String = "***u*u**uu*u*uu*u*u*u**uu*u*u*u*u********uuuuu*uuu*u*uuu*u*uuuu"

  val arbol2 = "**u*u*u*****uuuuu"
  val arbol3 = "***u*uu**u*uu"
  val arbol4 = "**u*u"
  val tree: Tree3 = Node3.string2Tree3(arbol4)
  println()
  TreeLayaut.layaut(tree)
  println(tree)
  println("número de * = " + arbol4.toList.count(_ == '*'))
  println()
  println("bMinus ================================================")
  println()
  println("tree = " + tree)
  println("bMinus(tree) = " + Tree3.bMinus(tree))





















}