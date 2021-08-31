val scalatestVer = "3.2.9"

lazy val firstItem = project
    .in(file("."))
    .settings(
        name := "retirCala",
        version := "0.1",
        scalaVersion := "2.13.6",
        libraryDependencies ++= Seq(
            "org.scalactic" %% "scalactic" % scalatestVer,
            "org.scalatest" %% "scalatest" % scalatestVer,
        )
    )

Compile / packageBin / mainClass := Some("retcalc.SimulatePlanApp")
