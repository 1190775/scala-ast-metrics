/* ---------- build.sbt (root of the metrics tool) ------------------ */
scalaVersion := "3.3.3"

/** TASTy-reflect / Inspector API */
libraryDependencies ++= Seq(
  "org.scala-lang"          %% "scala3-tasty-inspector" % scalaVersion.value,
  "org.scala-lang.modules"  %% "scala-xml"              % "2.2.0"   // ← NEW
)

