import aether.Aether._

aetherPublishBothSettings

publishMavenStyle := true

publishTo <<= version { v ⇒
  val proxy = "http://mavenproxy.finntech.no/finntech-internal-"
  val end = if(v endsWith "SNAPSHOT") "snapshot" else "release"
  Some("Finn-" + end at proxy + end)
}

