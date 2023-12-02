package app

import cats.syntax.all._
import com.monovore.decline.Opts
import java.nio.file.Path

object AppOpts {
  final case class Solve(day: Int, path: Option[Path])

  private lazy val dayOpts: Opts[Int] = Opts.argument[Int]("day")
  private lazy val pathOpts: Opts[Option[Path]] =
    Opts.option[Path]("path", "optionally provide the path to the input", "p", "path").orNone
  lazy val solve: Opts[Solve] =
    Opts.subcommand("solve", "solves the challenge for the given day")((dayOpts, pathOpts).tupled).map(Solve.apply)

}
