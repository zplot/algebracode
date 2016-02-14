package scala

import algebra._
import botany._

import  Utils._
import scala.language.implicitConversions




object TestMTree extends App {

  println("Empezamos Rooted Trees")

  val MT1 = MTree[Char]('a')

  val MT2 = MTree[Char]('a', List(MTree('f', List(MTree[Char]('g'))), MTree[Char]('c'), MTree('b', List(MTree[Char]('d'), MTree[Char]('e')))))

  println(MT1)
  println(MT2)

  val MT3 = MTree(MT2.toString)

  println(MT3)



}


