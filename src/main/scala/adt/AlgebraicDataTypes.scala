package adt

object AlgebraicDataTypes {

  final case class Suit private (value: Char) extends AnyVal
  object Suit{
    def create(value: Char): Option[Suit] = {
      if (List('a', 'c', 's', 'd').contains(value.toLower)) Some(Suit(value.toLower))
      else None
    }
  }

  final case class Rank private (value: Char) extends AnyVal
  object Rank{
    def create(value: Char): Option[Rank] = {
      val l = (1 to 9).map(_.toString).map(_(0)).toList ::: List('T', 'J', 'Q', 'K', 'A')
      if(l.contains(value.toUpper)) Some(Rank(value.toUpper))
      else None
    }
  }

  final case class Card(suit: Suit, rank: Rank)

  final case class Hand(cards: List[Card]){
    val combination: Combination = ???
  }
  object Hand {
    def createHoldem(cards: List[Card]): Option[Hand] =
      cards match {
        case cards if cards.size == 2   => Some(new Hand(cards))
        case _                          => None
      }
    def createOmaha(cards: List[Card]): Option[Hand] =
      cards match {
        case cards if cards.size == 4   => Some(new Hand(cards))
        case _                          => None
      }
  }

  final case class Board private (cards: List[Card])
  object Board {
    def create(cards: List[Card]): Option[Board] = {
      if (cards.length == 5) Some(Board(cards))
      else None
    }
  }

  sealed trait Combination
  object Combination {
    final case object HighCard extends Combination
    final case object Pair extends Combination
    final case object TwoPairs extends Combination
    final case object ThreeOfKind extends Combination
    final case object Straight extends Combination
    final case object Flush extends Combination
    final case object FullHouse extends Combination
    final case object FourOfKind extends Combination
    final case object StraightFlush extends Combination
    final case object RoyalFlush extends Combination
  }

  // Should return sorted hands list by strength
  def rankedHands(board: Board, hands: List[Hand], ordering: Ordering[Hand]): List[Hand] = ???

  // Should receive result of rankedHands as input
  final case class Result (board: Board, rankedHands: List[Hand])

}
