import sbt._
import Keys._

object BuildSettings {

  val org = "com.github.jrwest"
  val vsn = "0.0.0-SNAPSHOT"
  val scalaVsn = "2.9.1"

  val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := org,
    version := vsn,
    scalaVersion := scalaVsn,
    //shellPrompt := ShellPrompt.buildShellPrompt,
    scalacOptions ++= Seq("-deprecation", "-unchecked")
  )

}

object Dependencies {
  lazy val liftweb  = "net.liftweb" %% "lift-webkit" % "2.5-SNAPSHOT" % "compile" withSources()
  lazy val logback  = "ch.qos.logback" % "logback-classic" % "1.0.0" % "compile" withSources()
  lazy val specs2   = "org.specs2" %% "specs2" % "1.8.2" % "test" withSources()
  lazy val mockito  = "org.mockito" % "mockito-all" % "1.9.0" % "test" withSources()
  lazy val hamcrest = "org.hamcrest" % "hamcrest-all" % "1.1" % "test" withSources()
    
}

object ScalamachineBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val scalamachine = Project("scalamachine", file("."),
    settings = standardSettings,
    aggregate = Seq(core,lift)
  )

  lazy val core = Project("scalamachine-core", file("core"),
    settings = standardSettings ++
      Seq(
        name := "scalamachine-core",
        libraryDependencies ++= Seq(specs2,mockito,hamcrest)
      )
  )
  
  lazy val lift = Project("scalamachine-lift", file("lift"),
    dependencies = Seq(core), 
    settings = standardSettings ++
      Seq(
        name := "scalamachine-lift",
        libraryDependencies ++= Seq(liftweb)
      )
  )
  
  lazy val liftExample = Project("scalamachine-lift-example", file("examples/lift"),
    dependencies = Seq(lift),
    settings = standardSettings ++ 
      Seq(
        name := "scalamachine-lift-example",
        libraryDependencies ++= Seq(logback)
      )
  )

}

/*
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
    )

  val buildShellPrompt = {
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "sbt %s:%s> ".format (
        currProject, currBranch
      )
    }
  }
}
*/
