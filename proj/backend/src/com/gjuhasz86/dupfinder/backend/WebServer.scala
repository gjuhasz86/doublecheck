package com.gjuhasz86.dupfinder.backend

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer

import scala.annotation.tailrec
import scala.io.StdIn


object WebServer {
  def main(args: Array[String]) {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher


    val bindingFuture = Http().bindAndHandle(new Routes().route, "0.0.0.0", 4201)
    println(s"Server online at http://localhost:4201/\nType 'exit' without quotes and press RETURN to stop...")

    exitLoop()

    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }

  @tailrec
  def exitLoop(): Int = {
    val line = StdIn.readLine() // let it run until user presses return
    if (line == "exit") 0 else exitLoop()
  }
}
