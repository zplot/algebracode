package controllers

import models.GeneralEntryForm
import models.GeneralOutput
import models.MyHTML._
import models.MyClass._
import models.algebra.FiniteGroup
import models.algebra.FiniteGroupExamples._

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._




object General extends Controller {

  def inCaseOfErrors(key: String): GeneralEntryForm.Form1 = {

    val salida = key match {

      case "generic" => {


        val title: String = "Test"
        val title5fields: String = "Test"
        val label1: String = "Please review"
        val subject: String = "Test"
        val project: String = "Test"
        val references: String = "Test"
        val text1: String = "Test"
        val text2: String = "Test"

        GeneralEntryForm.Form1(
          key,
          title,
          title5fields,
          label1,
          subject,
          project,
          references,
          text1,
          text2
        )

      }

      case "cayley" => {

        val title: String = "Caley Table of a Permutation Group"
        val title5fields: String = "Caley Table of a Permutation Group"
        val label1: String = "Please review your input"
        val subject: String = "Permutation Groups"
        val project: String = "Cayley Table"
        val references: String = "These are the references on this subject"
        val text1: String = "What a permutation group is"
        val text2: String = "What a Cayley Table is"

        GeneralEntryForm.Form1(

          key,
          title,
          title5fields,
          label1,
          subject,
          project,
          references,
          text1,
          text2
        )
      }

    }

    salida
  }





  // Crea el form para introducir datos para cada aplicación identificada por key
  def createForm(key: String) = Action {

    val salida = key match {

      case "generic" => {
        val title: String = "Título de la página"
        val title5fields: String = "Este es el título 2 de la página"
        val label1: String = "Introduzca los datos"
        val subject: String = "Esto es el subject"
        val project: String = "Esto es el project"
        val references: String = "Estas son las references"
        val text1: String = "Esto entra en el campo text1"
        val text2: String = "Esto entra en el campo text2"

        val par = GeneralEntryForm.Form1(
          key,
          title,
          title5fields,
          label1,
          subject,
          project,
          references,
          text1,
          text2
        )
        par
      }

      case "cayley" => {

        val title: String = "Caley Table of a Permutation Group"
        val title5fields: String = "Caley Table of a Permutation Group"
        val label1: String = "Enter the name of a valid finite group"
        val subject: String = "Permutation Groups"
        val project: String = "Cayley Table"
        val references: String = "These are the references on this subject"
        val text1: String = "What a permutation group is"
        val text2: String = "What a Cayley Table is"

        val par = GeneralEntryForm.Form1(
          key,
          title,
          title5fields,
          label1,
          subject,
          project,
          references,
          text1,
          text2
        )
        par
      }
    }

    Ok(views.html.generalinput(salida))
  }

  // Aquí está la magia
  val input1fields = Form(mapping(

    "input1" -> nonEmptyText)(GeneralEntryForm.Form2.apply)(GeneralEntryForm.Form2.unapply))

  // Lanza la view que presenta los resultados
  def resultsView(key: String) = Action { implicit request =>
    input1fields.bindFromRequest.fold(

      // Hay errores
      formWithErrors => Ok(views.html.errorsview(inCaseOfErrors(key))),

      // No hay errores
      // A la view de presentación de resultados se le pasan los valores que debe presentar: process(key, value)
      value => Ok(views.html.generalblackboard(process(key, value))))

  }

    // Se le pasa un key que identifica la aplicación: key
    // Se le pasa los dotos introducidos por el usuario: value
    // Devuelve los datos que hay que mostrar en la página de resultados. Se guardan en: salida
    def process(key: String, value: GeneralEntryForm.Form2): GeneralOutput.PageContent = {

      val salida = key match {

        case "generic" => {

          val titular = "Este es el título de los resultados a mostrar"
          val numLineas = 5
          val contenido = List(List("1", "2", "3"), List("4", "5", "6"), List("7", "8", "9"))

          val key = "cayley"
          val title = "titulo"
          val notebook = "notebook"
          val page = "page"
          val next = "next"
          val previous ="previous"
          val subject = "subject"
          val project = "project"
          val references = "http://www.hp.com"
          val text1 = "text1"
          val text2 = "text2"

          val camposExtra = GeneralOutput.ExtraFields(
            key,
            title,
            notebook,
            page,
            next,
            previous,
            subject,
            project,
            references,
            text1,
            text2
          )

          GeneralOutput.PageContent(
            camposExtra,
            titular,
            numLineas,
            contenido
          )
        }

        case "cayley" => {

          def fromGroupStringToCayleyTableOK(s: String): Either[String, List[List[String]]]  = {
            val t = fromStringToGroup(s)
            t match {
              case Right(x)  => Right(x.cayleyTableOK)
              case Left(x) => Left(x)
            }

          }

          val titular = {
            val resultado: Either[String, List[List[String]]] = fromGroupStringToCayleyTableOK(value.input1)
            val output = resultado match {
              case Left(x) => x
              case Right(x) => "The Cayley table of selected group is:"
            }
            output
          }
          val numLineas = {
            val resultado: Either[String, List[List[String]]] = fromGroupStringToCayleyTableOK(value.input1)
            val output = resultado match {
              case Left(x) => 0
              case Right(x) => x.length
            }
            output
          }
          val contenido = {
            val resultado: Either[String, List[List[String]]] = fromGroupStringToCayleyTableOK(value.input1)
            val output = resultado match {
              case Left(x) => List(List())
              case Right(x) => x
            }
            output
          }

          val key = "cayley"
          val title = "Caley Table of a Permutation Group"
          val notebook = "Finite Groups"
          val page = "Cayley Table computation"
          val next = "next"
          val previous ="previous"
          val subject = "Permutation Groups"
          val project = "Cayley Table"
          val references = "http://www.hp.com"
          val text1: String = "A permutation group is a group G whose elements are permutations of a given set M and whose group operation is the composition of permutations in G (which are thought of as bijective functions from the set M to itself)."
          val text2: String = "If M = {1,2,...,n} then, S(M), the symmetric group on n letters is usually denoted by Sn. We write it in this site as S(n)"


          val camposExtra = GeneralOutput.ExtraFields(
            key,
            title,
            notebook,
            page,
            next,
            previous,
            subject,
            project,
            references,
            text1,
            text2
          )

          GeneralOutput.PageContent(
            camposExtra,
            titular,
            numLineas,
            contenido
          )
        }
      }

      salida

    }


  }






