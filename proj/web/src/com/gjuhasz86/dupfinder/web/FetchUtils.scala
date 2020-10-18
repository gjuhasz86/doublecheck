package com.gjuhasz86.dupfinder.web

import slinky.web.html._
import sttp.client._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object FetchUtils {

  def getBackend(apiPath: String)(callback: String => Unit): Unit = {
    println(s"Calling [$apiPath]")
    val request = basicRequest.get(uri"http://localhost:4201/api/$apiPath")

    implicit val backend = FetchBackend()
    request.send().onComplete {
      case Success(Response(Right(res), _, _, _, _)) =>
        callback(res)
      case _ =>
        println(s"Failed to send request [$apiPath] with [$body]")
    }
  }

  def postBackend(apiPath: String, body: String)(callback: String => Unit): Unit = {
    println(s"Calling [$apiPath] with [$body]")
    val request =
      basicRequest
        .body(body)
        .contentType("application/json")
        .post(uri"http://localhost:4201/api/$apiPath")

    implicit val backend = FetchBackend()
    request.send().onComplete {
      case Success(Response(Right(res), _, _, _, _)) =>
        callback(res)
      case _ =>
        println(s"Failed to send request [$apiPath] with [$body]")
    }
  }
}