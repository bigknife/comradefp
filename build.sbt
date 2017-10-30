import Dependencies._
import common._

scalaVersion in ThisBuild := "2.11.8"
scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.7",
  "-encoding",
  "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xfuture",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)

scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))

// customize version
/* if you wanna add git commit hash to artifact id, uncomment this
val gitHeadCommitSha = settingKey[String]("current git commit SHA")
val release          = settingKey[Boolean]("is publishing released version")
release := sys.props("release") == "true"

gitHeadCommitSha in ThisBuild := scala.sys.process.Process("git rev-parse HEAD").lines.head

version in ThisBuild := {
  val v = "0.1.0"
  if (release.value) v
  else s"$v-${gitHeadCommitSha.value}-SNAPSHOT"
}
 */
val release = settingKey[Boolean]("is publishing released version")
release := sys.props("release") == "true"
version in ThisBuild := {
  val v = "0.1.0"
  if (release.value) v
  else s"$v-SNAPTHOT"
}
// client project
lazy val rootProject = simpleProject
  .settings(
    ensimeIgnoreMissingDirectories := true
  )
