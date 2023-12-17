package days

import days.Card.Jack

class Day7(input: String) extends Puzzle {

  val hands = input.split("\n").map(Hand.apply)

  override def solve: String =
    val sortedHands = hands.sorted(Hand.ordering)
    sortedHands.zipWithIndex
      .map { case (card, i) =>
        card.bid.toLong * (i + 1)
      }
      .sum
      .toString

  override def solve2: String =
    val sortedHands = hands.sorted(Hand.ordering2)
    sortedHands.zipWithIndex
      .map { case (card, i) =>
        card.bid.toLong * (i + 1)

      }
      .sum
      .toString
}

object Day7:

end Day7

final case class Hand(cards: List[Card], bid: Int) {

  private val cardMap = cards
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  def rank(numberJokers: Int): Int = {
    // A map from the number of duplicates to how many of those there are
    // e.g. for hand AABBJ the map will be Map(2 -> 2, 1 -> 1)
    // and for hand 23456 the map will be Map(1 -> 5)
    val sizeMap = if (numberJokers == 0) {
      cardMap.groupMap(_._2)(_._1).view.mapValues(_.size).toMap
    } else {
      // If there are jokers we want to filter them out of the size map so we
      // can mor easily do the logic for the hand rank
      cardMap
        .filter(_._1 != Jack)
        .groupMap(_._2)(_._1)
        .view
        .mapValues(_.size)
        .toMap
    }

    if (sizeMap.contains(5 - numberJokers) || numberJokers == 5)
      7 // 5 of a kind
    else if (sizeMap.contains(4 - numberJokers))
      6 // 4 of a kind
    else if (
      (sizeMap.contains(3) && sizeMap.contains(2)) ||
      (sizeMap.getOrElse(2, 0) == 2 && numberJokers == 1)
    )
      5 // Full house
    else if (sizeMap.contains(3 - numberJokers))
      4 // 3 of a kind
    else if (sizeMap.getOrElse(2, 0) == 2)
      3 // two pair
    else if (sizeMap.contains(2) || numberJokers == 1)
      2 // pair
    else
      1 // High card
  }

  val rank: Int = rank(0)

  val rank2: Int = {
    val numberJokers = cardMap.getOrElse(Jack, 0)
    rank(numberJokers)
  }
}

object Hand {
  def apply(s: String): Hand =
    val parts = s.split(" ")
    Hand(parts(0).map(Card.apply).toList, parts(1).toInt)

  def ordering(
      handRank: Hand => Int,
      cardRank: Card => Int
  ): (Hand, Hand) => Int =
    (x: Hand, y: Hand) =>
      val rankCompare = handRank(x) - handRank(y)
      if (rankCompare != 0) rankCompare
      else {
        val cardCompares = x.cards.zip(y.cards).collectFirst {
          case (c1, c2) if c1 != c2 => cardRank(c1) - cardRank(c2)
        }
        cardCompares.get
      }

  val ordering: Ordering[Hand] =
    ordering(_.rank, _.rank)(_, _)

  val ordering2: Ordering[Hand] =
    ordering(_.rank2, _.rank2)(_, _)
}

sealed trait Card {
  def rank: Int
  def rank2: Int = rank
}

object Card {

  case object Ace extends Card {
    override val rank = 14
  }

  case object King extends Card {
    override val rank = 13
  }

  case object Queen extends Card {
    override val rank = 12
  }

  case object Jack extends Card {
    override val rank = 11
    override val rank2 = 1
  }

  case object Ten extends Card {
    override val rank = 10
  }

  case object Nine extends Card {
    override val rank = 9
  }

  case object Eight extends Card {
    override val rank = 8
  }

  case object Seven extends Card {
    override val rank = 7
  }

  case object Six extends Card {
    override val rank = 6
  }

  case object Five extends Card {
    override val rank = 5
  }

  case object Four extends Card {
    override val rank = 4
  }

  case object Three extends Card {
    override val rank = 3
  }

  case object Two extends Card {
    override val rank = 2
  }

  def apply(s: Char): Card = {
    s match {
      case 'A' => Ace
      case 'K' => King
      case 'Q' => Queen
      case 'J' => Jack
      case 'T' => Ten
      case '9' => Nine
      case '8' => Eight
      case '7' => Seven
      case '6' => Six
      case '5' => Five
      case '4' => Four
      case '3' => Three
      case '2' => Two
      case _   => throw new IllegalArgumentException(s"Invalid card: $s")
    }
  }
}
