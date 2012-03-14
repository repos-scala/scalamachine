name := "scalamachine"

organization := "com.github.jrwest"

version := "0.0.0-SNAPSHOT"

scalaVersion := "2.9.1"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.8.2" % "test" withSources(),
    "org.mockito" % "mockito-all" % "1.9.0" % "test" withSources(),
    "org.hamcrest" % "hamcrest-all" % "1.1" % "test" withSources()
)

scalacOptions := Seq("-unchecked", "-deprecation")

