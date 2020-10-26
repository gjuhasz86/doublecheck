package com.gjuhasz86.dupfinder.web

import org.scalajs.dom.ext.Ajax

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FetchUtils {
  val location = org.scalajs.dom.window.location

  def getBackend(apiPath: String)(callback: String => Unit): Unit = {
    println(s"Calling [$apiPath]")
    Ajax.get(s"/api/$apiPath")
      .foreach { xhr => callback(xhr.responseText) }
  }

  def getPath(apiPath: String): Future[String] = {
    println(s"Calling [$apiPath]")
    Ajax.get(s"/api/$apiPath").map(_.responseText)
  }

  def postBackend(apiPath: String, body: String)(callback: String => Unit): Unit = {
    println(s"Calling [$apiPath] with [$body]")
    Ajax.post(s"/api/$apiPath", body, headers = Map("Content-Type" -> "application/json"))
      .foreach { xhr => callback(xhr.responseText) }
  }

  def postPath(apiPath: String, body: String) = {
    println(s"Calling [$apiPath] with [$body]")
    Ajax.post(s"/api/$apiPath", body, headers = Map("Content-Type" -> "application/json"))
      .map(_.responseText)
  }
}