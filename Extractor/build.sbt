import AssemblyKeys._ // put this at the top of the file

name := "Extractor"

version := "1.0"

scalaVersion := "2.11.7"
val sparkVersion = "2.1.1"

libraryDependencies ++= Seq(

  "org.apache.logging.log4j" % "log4j-core" % "2.7",
  "org.apache.logging.log4j" % "log4j-api" % "2.7",
  "org.apache.logging.log4j" %% "log4j-api-scala" % "2.8.1",

  "log4j" % "log4j" % "1.2.17",
  "net.ruippeixotog" %% "scala-scraper" % "1.2.0",
  "org.jsoup"                   % "jsoup"                % "1.10.1",
  //"org.json4s" %% "json4s-native" % "3.5.1",
  "org.json" % "json" % "20160810",
  "org.apache.commons" % "commons-configuration2" % "2.0",
  "commons-beanutils" % "commons-beanutils" % "1.9.3",
  "org.postgresql" % "postgresql" % "42.0.0.jre7",
  "com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "org.apache.commons" % "commons-text" % "1.1",
  "org.apache.derby" % "derby" % "10.13.1.1",
  //"org.postgresql" % "postgresql" % "42.2.4",
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",

"org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "org.apache.commons" % "commons-lang3" % "3.0",
  "org.apache.commons" % "commons-vfs2" % "2.0"
)

assemblySettings
jarName in assembly := "extractor.jar"
//mainClass in assembly := Some("Example")