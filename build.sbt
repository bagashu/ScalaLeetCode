name := "scala_leet"

version := "1.0"

scalaVersion := "2.12.1"

//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
//libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"
//libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases" +  "http://repo1.maven.org/maven2"
 val akkaVersion = "2.4.20"

libraryDependencies ++= Seq(
  "org.scala-js" %% "scalajs-test-interface" % "0.6.14",
  "org.scalatest" %% "scalatest" % "3.0.1", //version changed as these the only versions supported by 2.12
  "com.novocode" % "junit-interface" % "0.11",
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "com.typesafe.akka"          %% "akka-actor"                    % akkaVersion,
  "com.typesafe.akka"          %% "akka-agent"                    % akkaVersion,
  "com.typesafe.akka"          %% "akka-cluster"                  % akkaVersion,
  "com.typesafe.akka"      %% "akka-cluster-sharding"         % akkaVersion


)