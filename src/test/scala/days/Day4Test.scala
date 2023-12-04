package days

import days.Day4._
import munit.FunSuite

class Day4Test extends FunSuite {
  test("Card.prizeCards") {
    val card = Card(1, Set(1, 2, 3), Set(2, 3, 4, 5))
    assertEquals(card.prizeCards, List(2, 3))
  }
}
