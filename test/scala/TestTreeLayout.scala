package scala


object TestTreeLayout extends App {

  import models.botany._



  println("Empezamos")

  val leaf1 = Node3(Vector[Node3]())
  val leaf2 = Node3(Vector[Node3]())
  println(leaf1 == leaf2)
  val node1 = Node3(Vector(leaf1, leaf1, leaf1))
  val tree1 = Tree3(node1)

  println(tree1)













}