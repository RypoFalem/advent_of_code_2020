name := "advent_of_code_2020"

version := "0.1"

scalaVersion := "2.13.4"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
