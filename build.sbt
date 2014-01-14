
organization  := "com.gocatch"

name          := "problems"

version       := "0.1"

scalaVersion  := "2.10.3"

libraryDependencies ++= Seq(
  "org.specs2"      %% "specs2-core"        % "2.3.7"   % "test",
  "org.specs2"      %% "specs2-scalacheck"  % "2.3.7"   % "test",
  // "org.specs2"      %% "specs2-matcher"    % "2.3.7"   % "test",
  "org.scalacheck"  %% "scalacheck"         % "1.10.1"  % "test",
  "org.scalaz"      %% "scalaz-core"        % "7.0.5"   % "compile")

scalacOptions in Test ++= Seq("-Yrangepos")

// vim: set ts=2 sw=2 et:
// sublime: tab_size 2; translate_tabs_to_spaces true
