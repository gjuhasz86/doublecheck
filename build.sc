import mill._, scalalib._, mill.scalajslib._


trait Commmon extends ScalaModule {
  def scalaVersion = "2.13.3"
}

object proj extends Module {
  object shared extends ScalaJSModule with Commmon {
    override def scalaJSVersion = "1.3.0"

  }

  object backend extends ScalaModule with Commmon {
    override def moduleDeps = Seq(shared)
    override def ivyDeps = Agg(
      ivy"com.typesafe.akka::akka-http:10.2.1",
      ivy"com.typesafe.akka::akka-stream:2.6.10",
      ivy"ch.qos.logback:logback-classic:1.2.3",
      ivy"com.typesafe.scala-logging::scala-logging:3.9.2"
    )
  }

  object web extends ScalaJSModule with Commmon {
    override def moduleDeps = Seq(shared)

    override def scalaJSVersion = "1.3.0"

    override def scalacOptions = List(
      "-language:higherKinds",
      "-Ymacro-annotations"
    )

    override def ivyDeps = Agg(
      ivy"org.scala-js:scalajs-dom_sjs1_2.13:1.1.0",
      ivy"com.lihaoyi:scalatags_sjs1_2.13:0.9.2",
      ivy"me.shadaj:slinky-web_sjs1_2.13:0.6.6",
      ivy"me.shadaj:slinky-core-ijext_2.13:0.6.6"
    )

  }

  def dist = T {
    if (!os.exists(os.pwd / 'dist)) {os.makeDir(os.pwd / 'dist)}
    if (!os.exists(os.pwd / 'dist / 'public)) {os.makeDir(os.pwd / 'dist / 'public)}
    val serverJar = proj.backend.assembly()
    val jsFile = proj.web.fastOpt()
    val jsFileDir = jsFile.path.toNIO.getParent
    val jsFileName = jsFile.path.last

    println(s"Copying [${serverJar.path}]")
    println(s"Copying [${jsFile.path}]")
    println(s"Copying [${os.Path(jsFileDir.toString) / s"$jsFileName.map"}]")
    os.copy.over(serverJar.path, os.pwd / 'dist / serverJar.path.last)
    os.copy.over(jsFile.path, os.pwd / 'dist / 'public / jsFileName)
    os.copy.over(os.Path(jsFileDir.toString) / s"$jsFileName.map", os.pwd / 'dist / 'public / s"$jsFileName.map")

    os.walk(os.pwd / 'proj / 'public)
      .map(_.relativeTo(os.pwd / 'proj / 'public))
      .foreach { f =>
        println(s"Copying [${os.pwd / 'proj / 'public / f}]")
        os.copy.over(os.pwd / 'proj / 'public / f, os.pwd / 'dist / 'public / f)
      }

  }
}