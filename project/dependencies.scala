import sbt._

object Dependencies {
  object versions {
    val simulacrum    = "0.10.0"
    val cats          = "0.9.0"
    val catsEffect    = "0.3"
    val kindProjector = "0.9.4"
    val scalaMeta     = "3.0.0-M7"
    val scalaMacro    = "2.1.0"
    val freestyle     = "0.3.1"
    val fs2           = "0.9.7"
    val fs2Cat        = "0.3.0"
    val scalaTest     = "3.0.1"
    val scalaCheck    = "1.13.4"
    val scopt         = "3.6.0"
    val http4s        = "0.17.4"
    val circe         = "0.8.0"
    val doobie        = "0.4.4"
    val servlet       = "2.5"
    val hikariCP      = "2.7.1"
    val logback       = "1.2.2"
    val config        = "1.3.1"
  }

  object libraries {
    val simulacrum = Seq("com.github.mpilquist" %% "simulacrum" % versions.simulacrum)

    val cats = Seq(
      "org.typelevel" %% "cats"        % versions.cats,
      "org.typelevel" %% "cats-effect" % versions.catsEffect
    )

    val freestyle = Seq(
      "io.frees" %% "freestyle",
      "io.frees" %% "freestyle-effects",
      "io.frees" %% "freestyle-fs2"
    ).map(_ % versions.freestyle)

    val fs2 = Seq(
      "co.fs2" %% "fs2-core" % versions.fs2,
      "co.fs2" %% "fs2-io"   % versions.fs2,
      "co.fs2" %% "fs2-cats" % versions.fs2Cat
    )

    val unitTest = Seq(
      "org.scalactic"  %% "scalactic"  % versions.scalaTest,
      "org.scalatest"  %% "scalatest"  % versions.scalaTest % "test",
      "org.scalacheck" %% "scalacheck" % versions.scalaCheck % "test"
    )

    val scopt = Seq("com.github.scopt" %% "scopt" % versions.scopt)

    val http4s_server = Seq(
      "org.http4s" %% "http4s-dsl",
      "org.http4s" %% "http4s-blaze-server",
      "org.http4s" %% "http4s-blaze-client",
      "org.http4s" %% "http4s-circe",
      "org.http4s" %% "http4s-servlet"
    ).map(_ % versions.http4s)

    val doobie = Seq(
      "org.tpolecat" %% "doobie-core-cats",
      "org.tpolecat" %% "doobie-hikari-cats"
    ).map(_        % versions.doobie) ++ Seq(
      "com.zaxxer" % "HikariCP" % versions.hikariCP
    )

    val circe = Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-literal",
      "io.circe" %% "circe-parser"
    ).map(_ % versions.circe)

    val servletApi = Seq(
      "javax.servlet" % "servlet-api" % versions.servlet % "provided"
    )

    val logback = Seq("ch.qos.logback" % "logback-classic" % versions.logback)

    val typesafeConfig = Seq("com.typesafe" % "config" % versions.config)
  }

  object compilerPlugins {
    val scalaMacro = Seq(
      "org.scalamacros" % "paradise" % versions.scalaMacro cross CrossVersion.full)
    val scalaMeta     = Seq("org.scalameta"  % "paradise"        % versions.scalaMeta cross CrossVersion.patch)
    val kindProjector = Seq("org.spire-math" %% "kind-projector" % versions.kindProjector)
  }

  object projects {
    import libraries._
    // add your projects libraryDependencies here
    private val common = logback ++ unitTest ++ cats ++ freestyle ++ fs2 ++ typesafeConfig
    val server         = common ++ circe ++ http4s_server ++ servletApi
    val admin          = common ++ circe ++ http4s_server ++ servletApi
  }
}
