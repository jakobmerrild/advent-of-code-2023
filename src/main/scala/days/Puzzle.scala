package days

import cats.effect.ExitCode
import cats.effect.IO

trait Puzzle {
  def solve: String
  def solve2: String
}
