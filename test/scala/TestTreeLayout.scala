package scala


object TestTreeLayout extends App {

  import models.botany._



  println("Empezamos")

  val leaf1 = Node3(Vector[Node3]())
  val leaf2 = Node3(Vector[Node3]())
  val leaf3 = Node3(Vector[Node3]())
  println(leaf1 == leaf2)
  val node1 = Node3(Vector(leaf1, leaf2, leaf3))
  val tree1: Tree3 = Tree3(node1)

  println(tree1)
  TreeLayaut.layaut(tree1)
  println(leaf1.x)
  println(leaf1.y)
  println(leaf2.x)
  println(leaf2.y)
  println(leaf3.x)
  println(leaf3.y)
  println(node1.x)
  println(node1.y)

  println("Capitulo 1")
  println()
  val string = "**u*u*u"
  val tree2: Tree3 = Node3.string2Tree3(string)
  println(tree2)
  TreeLayaut.layaut(tree2)
















}