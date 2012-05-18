import sbt._
import com.github.siasia._
import WebPlugin._
import Keys._

object BuildSettings {

  val org = "com.github.jrwest"
  val vsn = "0.0.0-SNAPSHOT"
  val scalaVsn = "2.9.1"

  lazy val publishSetting = publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }


  lazy val publishSettings = Seq(
    publishSetting,
    publishMavenStyle := true,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/jrwest/scalamachine/wiki</url>
        <licenses>
          <license>
            <name>Apache 2</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:jrwest/scalamachine.git</url>
          <connection>scm:git:git@github.com:jrwest/scalamachine.git</connection>
        </scm>
        <developers>
          <developer>
            <id>jrwest</id>
            <name>Jordan West</name>
            <url>http://github.com/jrwest</url>
          </developer>
        </developers>
      )
  )

  val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := org,
    version := vsn,
    scalaVersion := scalaVsn,
    resolvers += ("twitter repository" at "http://maven.twttr.com"),
    shellPrompt <<= ShellPrompt.prompt,
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    testOptions in Test += Tests.Argument("html console"),
    publishArtifact in Test := false,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  )

}

object Dependencies {
  lazy val internalScalaz = "com.github.jrwest"       %% "scalamachine-scalaz-core" % "7.0-SNAPSHOT"    % "compile" withSources()
  lazy val scalaz7        = "org.scalaz"              %% "scalaz-core"              % "7.0-SNAPSHOT"    % "compile" withSources()
  lazy val scalaz6        = "org.scalaz"              %% "scalaz-core"              % "6.0.3"           % "compile" withSources()
  lazy val slf4j          = "org.slf4j"               % "slf4j-api"                 % "1.6.4"           % "compile"
  // Don't want to keep t his dependency long term but for now its fastest way to get date parsing for http
  lazy val commonsHttp    = "commons-httpclient"      % "commons-httpclient"        % "3.1"                        withSources()
  lazy val liftweb        = "net.liftweb"             %% "lift-webkit"              % "2.4"             % "compile" withSources()
  lazy val jetty          = "org.eclipse.jetty"       % "jetty-webapp"              % "7.3.0.v20110203" % "container"
  lazy val finagle        = "com.twitter"             %% "finagle-http"             % "1.9.12"          % "compile" withSources()
  lazy val logback        = "ch.qos.logback"          % "logback-classic"           % "1.0.0"           % "compile" withSources()
  lazy val specs2         = "org.specs2"              %% "specs2"                   % "1.9"             % "test" withSources()
  lazy val scalacheck     = "org.scala-tools.testing" %% "scalacheck"               % "1.9"             % "test" withSources()
  lazy val mockito        = "org.mockito"             % "mockito-all"               % "1.9.0"           % "test" withSources()
  lazy val hamcrest       = "org.hamcrest"            % "hamcrest-all"              % "1.1"             % "test" withSources()
  lazy val pegdown        = "org.pegdown"             % "pegdown"                   % "1.0.2"           % "test"
}

object ScalamachineBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val scalamachine = Project("scalamachine", file("."),
    settings = standardSettings ++ publishSettings ++ Seq(publishArtifact in Compile := false),
    aggregate = Seq(core,scalaz6utils,scalaz7utils,lift,netty)
  )

  lazy val core = Project("scalamachine-core", file("core"),
    settings = standardSettings ++ publishSettings ++
      Seq(
        name := "scalamachine-core",
        libraryDependencies ++= Seq(internalScalaz,slf4j,commonsHttp,specs2,scalacheck,mockito,hamcrest,pegdown)
      )
  )

  lazy val scalaz6utils = Project("scalamachine-scalaz6", file("scalaz6"),
    dependencies = Seq(core),
    settings = standardSettings ++ publishSettings ++
      Seq(
        name := "scalamachine-scalaz6",
        libraryDependencies ++= Seq(scalaz6)
      )
  )

  lazy val scalaz7utils = Project("scalamachine-scalaz7", file("scalaz7"),
    dependencies = Seq(core),
    settings = standardSettings ++ publishSettings ++
      Seq(
        name := "scalamachine-scalaz7",
        libraryDependencies ++= Seq(scalaz7)
      )
  )
  
  lazy val lift = Project("scalamachine-lift", file("lift"),
    dependencies = Seq(core), 
    settings = standardSettings ++ publishSettings ++
      Seq(
        name := "scalamachine-lift",
        libraryDependencies ++= Seq(liftweb)
      )
  )

  lazy val netty = Project("scalamachine-netty", file("netty"),
    dependencies = Seq(core),
    settings = standardSettings ++ publishSettings ++
      Seq(
        name := "scalamachine-netty",
        libraryDependencies ++= Seq(finagle)
      )
  )
  
  lazy val liftExample = Project("scalamachine-lift-example", file("examples/lift"),
    dependencies = Seq(lift),
    settings = standardSettings ++ webSettings ++
      Seq(
        name := "scalamachine-lift-example",
        libraryDependencies ++= Seq(jetty,logback)
      )
  )

  lazy val finagleExample = Project("scalamachine-finagle-example", file("examples/finagle"),
    dependencies = Seq(netty),
    settings = standardSettings ++
      Seq(
        name := "scalamachine-finagle-example",
        libraryDependencies ++= Seq(logback)
      )
  )

}

object ShellPrompt {
  val prompt = name(name => { state: State =>
    object devnull extends ProcessLogger {
      def info(s: => String) {}
      def error(s: => String) { }
      def buffer[T](f: => T): T = f
    }
    val current = """\*\s+(\w+)""".r
    def gitBranches = ("git branch --no-color" lines_! devnull mkString)
    "%s | %s> " format (
      name,
      current findFirstMatchIn gitBranches map (_.group(1)) getOrElse "-"
      )
  })
}

