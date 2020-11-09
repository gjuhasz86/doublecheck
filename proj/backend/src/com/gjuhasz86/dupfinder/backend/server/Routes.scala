package com.gjuhasz86.dupfinder.backend.server

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.gjuhasz86.dupfinder.backend.core.GraphBuilder
import com.gjuhasz86.dupfinder.backend.core.Node
import com.gjuhasz86.dupfinder.backend.server.Syntax._
import com.gjuhasz86.dupfinder.shared.NodeLite
import com.gjuhasz86.dupfinder.shared.NodeType
import com.gjuhasz86.dupfinder.shared.Stat
import com.gjuhasz86.dupfinder.shared.Stats
import com.gjuhasz86.dupfinder.shared.request.NodeReq
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.Encoder

case class Foo(bar: String)
class Routes(staticPath: String, graphBuilder: GraphBuilder) extends LazyLogging {
  import io.circe.generic.auto._

  implicit val encodeNode: Encoder[Node] =
    Encoder.forProduct11("ntype", "path", "name", "size", "hash", "stats", "dupCount", "extDupCount", "dummyCount", "childCount", "selfDupCount")(n =>
      (n.ntype.short, n.path, n.name, n.size, n.hash, n.stats.map { case (k, v) => k.short -> v }, n.dupCount, n.extDupCount, n.dummyCount, n.childCount, n.selfDupCount)
    )

  val route = apiRoute ~ staticRoute

  lazy val apiRoute: Route = {
    p("api") {
      _.get("root") {
        complete(graphBuilder.root)
      }.get("rootLite") {
        complete(graphBuilder.root.toLite)
      }.post("children") {
        entity(as[String]) { path =>
          println(s"Children of [$path]")
          val res = graphBuilder.nodesByPath.get(path).toList.flatMap(_.children)
          println(s"Returning [${res.size}]")
          complete(res)
        }
      }.post("search") {
        entity(as[NodeReq]) { req =>
          println(s"Searching [$req]")
          val res = graphBuilder.search(req).map(n => graphBuilder.nodesByPath(n.path))
          println(s"Returning [${res.size}]")
          complete(res.toList)
        }
      }.post("searchLite") {
        entity(as[NodeReq]) { req =>
          println(s"Searching [$req]")
          val res = graphBuilder.search(req)
          println(s"Returning [${res.size}]")
          complete(res.toList)
        }
      }.post("dups") {
        entity(as[List[String]]) { hashes =>
          println(s"Dups of [$hashes]")
          val res = hashes.distinct.flatMap(graphBuilder.pathsByHash.get).flatten.distinct.map(graphBuilder.nodesByPath)
          println(s"Returning [${res.size}]")
          complete(res)
        }
      }.post("dups2") {
        entity(as[List[String]]) { hashes =>
          println(s"Dups of [$hashes]")
          val res = graphBuilder.dups0(hashes).map(_.toLite)
          println(s"Returning [${res.size}]")
          complete(res)
        }
      }.post("allext") {
        entity(as[String]) { path =>
          println(s"All ext dups of [$path]")
          val res =
            graphBuilder.nodesByPath.get(path)
              .map(_.externalDupPaths.distinct)
              .getOrElse(Nil)
              .flatMap(graphBuilder.nodesByPath.get)

          println(s"Returning [${res.size}]")
          complete(res)
        }
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


