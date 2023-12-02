package input

import cats.effect.IO
import app.AppOpts.Solve
import cats.effect.kernel.Resource
import java.io.InputStream
import scala.io.BufferedSource

object Reader {
  def getInputForDay(args: Solve): IO[String] = {
    val resource = args.path.fold {
      Resource.fromAutoCloseable[IO, BufferedSource](IO(io.Source.fromResource(s"${args.day}/input.txt")))
    } { path =>
      Resource.fromAutoCloseable[IO, BufferedSource](IO(io.Source.fromFile(path.toFile(), 1024)))
    }
    resource.use { source =>
      IO(source.getLines().mkString("\n"))
    }
  }
}
