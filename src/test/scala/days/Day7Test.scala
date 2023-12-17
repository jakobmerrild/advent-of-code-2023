package days

import munit.{Fail, FunSuite}

class Day7Test extends FunSuite {
  test("Hand.rank") {
    val fiveOfAKind = Hand("22222 12")
    val fourOfAKind = Hand("AA2AA 50")
    val fullHouse = Hand("32233 1")
    val threeOfAKind = Hand("22452 1")
    val twoPair = Hand("JJKK2 12")
    val pair = Hand("TT975 11")
    val high = Hand("A4567 16")
    assertEquals(fiveOfAKind.rank > fourOfAKind.rank, true)
    assertEquals(fourOfAKind.rank > fullHouse.rank, true)
    assertEquals(fullHouse.rank > threeOfAKind.rank, true)
    assertEquals(threeOfAKind.rank > twoPair.rank, true)
    assertEquals(twoPair.rank > pair.rank, true)
    assertEquals(pair.rank > high.rank, true)

    val descendingOrder = List(
      fiveOfAKind,
      fourOfAKind,
      fullHouse,
      threeOfAKind,
      twoPair,
      pair,
      high
    )
    val ascendingOrder = descendingOrder.reverse

    assertEquals(descendingOrder.sorted(Hand.ordering), ascendingOrder)
  }

  test("Hand.rank2") {
    val fiveOfAKind = Hand("JAJAJ 1")
    val fourOfAKind = Hand("J111A 1")
    val fullHouse = Hand("AAJKK 34")
  }

  test("solve(2)") {
    val input =
      """32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483""".stripMargin

    val day7 = new Day7(input)
    assertEquals(day7.solve, "6440")
    assertEquals(day7.solve2, "5905")
  }
}
