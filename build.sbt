val toolkitV = "0.5.0"
val toolkit = "org.scala-lang" %% "toolkit" % toolkitV
val toolkitTest = "org.scala-lang" %% "toolkit-test" % toolkitV

ThisBuild / scalaVersion := "3.3.4"
libraryDependencies += toolkit
libraryDependencies += (toolkitTest % Test)
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

lazy val format = taskKey[Unit]("format all files")
format := Def
  .sequential(
    Compile / scalafmtAll,
    Compile / scalafmtSbt,
  )
  .value
