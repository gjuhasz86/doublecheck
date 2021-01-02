package com.gjuhasz86.dupfinder.backend.server

import java.time.LocalDateTime

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.gjuhasz86.dupfinder.backend.core.GraphBuilder
import com.gjuhasz86.dupfinder.backend.server.Syntax._
import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.Stat
import com.gjuhasz86.dupfinder.shared.StatKey
import com.gjuhasz86.dupfinder.shared.request.MarkReq
import com.gjuhasz86.dupfinder.shared.request.MarkType
import com.gjuhasz86.dupfinder.shared.request.MarkType.Deleted
import com.gjuhasz86.dupfinder.shared.request.MarkType.Ignored
import com.gjuhasz86.dupfinder.shared.request.MarkType.Saved
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder

case class Foo(bar: String)
class Routes(staticPath: String, graphBuilder: GraphBuilder) extends LazyLogging {
  import io.circe.generic.auto._

  implicit def skEnc[T]: KeyEncoder[StatKey[T]] = _.name
  implicit val skDec: KeyDecoder[StatKey[_]] = StatKey.of(_)

  val route = apiRoute ~ staticRoute

  lazy val apiRoute: Route = {
    p("api") {
      _.get("rootLite") {
        println(s"Building root")
        val res = graphBuilder.enrichNode(graphBuilder.root)
        println(s"Returning [$res]")
        complete(res)
      }.post("searchLite") {
        entity(as[NodeReq]) { req =>
          println(s"Searching [$req]")
          val res = graphBuilder.search(req)
          println(s"Returning [${res.size}]")
          complete(res.toList)
        }
      }.post("mark") {
        entity(as[MarkReq]) {
          case MarkReq(markType, nodes) =>
            complete(graphBuilder.mark(markType, nodes))
        }
      }.post("getmarked") {
        entity(as[MarkType]) {
          case Ignored => complete(graphBuilder.ignored.toList)
          case Deleted => complete(graphBuilder.deleted.toList)
          case Saved => complete(graphBuilder.saved.toList)
        }
      }.post("persistmarks") {
          complete(graphBuilder.persistMarks())
      }.get("debug") {
        println(s"calc seq extdupcount... ${LocalDateTime.now}")
        val res = graphBuilder.root.children.map(n => graphBuilder.extDupCount(n))
        println(LocalDateTime.now)
        println(res)

//        graphBuilder.externalDups(graphBuilder.root)
        complete("ok")
      }
    }
  }

  lazy val staticRoute = {
    pathEndOrSingleSlash {
      getFromFile(s"$staticPath/index.html")
    } ~
      getFromBrowseableDirectory(staticPath)

  }

}


