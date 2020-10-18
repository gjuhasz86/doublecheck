package com.gjuhasz86.dupfinder.backend

import akka.http.scaladsl.server.Directives._
import com.typesafe.scalalogging.LazyLogging
import akka.http.scaladsl.server.Route
import com.gjuhasz86.dupfinder.backend.Syntax._

class Routes() extends LazyLogging {


  val route = apiRoute ~ staticRoute

  lazy val apiRoute: Route = {
    p("api") {
      _.get("root") {
        complete("root path")
      }.post("children") {
        entity(as[String]) { path =>
          println(s"Children of [$path]")

          complete(s"children of [$path]")
        }
      }
    }
  }

  lazy val staticRoute = {
    pathEndOrSingleSlash {
      getFromFile("public/index.html")
    } ~
      getFromBrowseableDirectory("public")

  }
}


