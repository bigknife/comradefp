import sbt._, Keys._

import Dependencies._
import com.earldouglas.xwp.{JettyPlugin, TomcatPlugin}

object common {
  val commonSettings = Seq(
    organization := "ues2",
    version := "0.0.1",
    publishArtifact in (Compile, packageDoc) := false,
    sources in (Compile, doc) := Seq.empty
  )

  val publishSettings = Seq(
    credentials := Seq(Credentials(Path.userHome / ".dev" / "weihui.credentials")),
    pomIncludeRepository := { _ ⇒
      false
    },
    publishTo := {
      val nexus = "http://nexus.weihui.com:8081/"
      if (isSnapshot.value)
        Some("weihui_snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("weihui_releases" at nexus + "content/repositories/releases")
    }
  )

  def publishableProject(name: String): Project =
    Project(name, file(name))
      .settings(commonSettings: _*)
      .settings(publishSettings: _*)

  def webProject(name: String): Project =
    Project(name, file(name))
      .enablePlugins(JettyPlugin, TomcatPlugin)
      .settings(commonSettings: _*)

  def simpleProject: Project =
    Project("root", file("."))
      .settings(commonSettings: _*)

}
