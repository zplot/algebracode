package models

// Este es el modelo de datos de la página de presentación de resultados

import scala.language.implicitConversions

object GeneralOutput {

  case class ExtraFields(
                              key: String,
                              title: String,
                              notebook: String,
                              page: String,
                              next: String,
                              previous: String,
                              subject: String,
                              project: String,
                              references: String,
                              text1: String,
                              text2: String
                            )

  case class PageContent(
                              extraFields: ExtraFields, // campos auxiliares de la página
                              titular: String, // Titular para la parte de arriba de la pizarra
                              lineas: Int,
                              contenido: List[List[String]] // Resultados

                            )



}


