namespace ProjectEulerTests

open NUnit.Framework
open FsUnit
open PE54

[<TestFixture>]
type PE54Tests() = class
    
    [<Test>]
    member this.``When creating a hand from cards returns a valid hand``() =
        let card1 = ValueCard(5, Heart)
        let card2 = ValueCard(5, Club)
        let card3 = ValueCard(6, Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = King(Diamond)
        let expected = [card1; card2; card3; card4; card5]

        let hand = new Hand(card1, card2, card3, card4, card5)
        let actual = hand.Cards

        actual |> should equal expected

    [<Test>]
    member this.``When creating a hand from string returns a valid hand``() =
        let handAsString = "5H 5C 6S 7S KD"
        let expected = [ValueCard(5, Heart); ValueCard(5, Club); ValueCard(6, Spade); ValueCard(7, Spade); King(Diamond)]

        let hand = new Hand(handAsString)
        let actual = hand.Cards

        actual |> should equal expected

    [<Test>]
    member this.``When creating a hand from string not containg 5 cards throws``() =
        let handAsString = "5H 5C 6S 7S"

        (fun () -> new Hand(handAsString) |> ignore) |> should throw typeof<System.ArgumentException>

    [<Test>]
    member x.``When getting a suit from a valid card as a string returns a valid suit``() =
        let cardAsString = "QS"
        let expected = Spade

        let actual = getSuitFromCard cardAsString

        actual |> should equal expected

    [<Test>]
    member x.``When getting a suit from an empty string should throw argument exception``() =
        let cardAsString = ""

        (fun () -> getSuitFromCard cardAsString |> ignore) |> should throw typeof<System.ArgumentException>

    [<Test>]
    member x.``When getting a suit from an invalid string should throw``() =
        let cardAsString = "3X"

        (fun () -> getSuitFromCard cardAsString |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    member x.``When creating a card from valid string should return a valid card``() =
        let cardAsString = "9S"
        let expected = ValueCard(9, Spade)

        let actual = stringToCard cardAsString

        actual |> should equal expected

    [<Test>]
    member x.``When creating a ten card from valid string should return a valid card``() =
        let cardAsString = "TS"
        let expected = ValueCard(10, Spade)

        let actual = stringToCard cardAsString

        actual |> should equal expected

    [<Test>]
    member x.``When creating a card from invalid string should throw``() =
        let cardAsString = "9X"

        (fun () -> stringToCard cardAsString |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    member x.``When creating a card from invalid rank should throw``() =
        let cardAsString = "10C"

        (fun () -> stringToCard cardAsString |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    member x.``When creating a card from empty string should throw argument exception``() =
        let cardAsString = ""

        (fun () -> stringToCard cardAsString |> ignore) |> should throw typeof<System.ArgumentException>

    [<Test>]
    member x.``When getting poker hand from a hand containing a pair returns pair``() =
        let card1 = ValueCard(5, Heart)
        let card2 = ValueCard(5, Club)
        let card3 = ValueCard(6, Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = King(Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = OnePair(3)

        let actual = hand.GetPokerHand()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing a pair returns pair``() =
        let card1 = ValueCard(5, Heart)
        let card2 = ValueCard(5, Club)
        let card3 = ValueCard(6, Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = King(Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([2;1;1;1],[3;11;5;4])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing two pairs returns two pairs``() =
        let card1 = ValueCard(5, Heart)
        let card2 = ValueCard(5, Club)
        let card3 = King(Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = King(Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([2;2;1],[11;3;5])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing three of a kind returns correct score``() =
        let card1 = ValueCard(5, Heart)
        let card2 = King(Club)
        let card3 = King(Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = King(Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([3;1;1],[11;5;3])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing four of a kind returns correct score``() =
        let card1 = King(Heart)
        let card2 = King(Club)
        let card3 = King(Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = King(Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([4;1],[11;5])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing full house returns correct score``() =
        let card1 = ValueCard(5, Heart)
        let card2 = King(Club)
        let card3 = King(Spade)
        let card4 = ValueCard(5, Spade)
        let card5 = King(Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([3;2],[11;3])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing high card returns correct score``() =
        let card1 = ValueCard(5, Heart)
        let card2 = King(Club)
        let card3 = Queen(Spade)
        let card4 = ValueCard(7, Spade)
        let card5 = ValueCard(10, Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([1;1;1;1;1],[11;10;8;5;3])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing straight returns correct score``() =
        let card1 = ValueCard(5, Heart)
        let card2 = ValueCard(6, Spade)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(8, Spade)
        let card5 = ValueCard(9, Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([3;1;2],[7;6;5;4;3])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing a straight from ace returns correct score``() =
        let card1 = Ace(Heart)
        let card2 = ValueCard(2, Spade)
        let card3 = ValueCard(3, Spade)
        let card4 = ValueCard(4, Spade)
        let card5 = ValueCard(5, Diamond)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([3;1;2],[3;2;1;0;-1])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing flush returns correct score``() =
        let card1 = ValueCard(2, Spade)
        let card2 = ValueCard(6, Spade)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(8, Spade)
        let card5 = ValueCard(9, Spade)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([3;1;3],[7;6;5;4;0])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When getting hand score from a hand containing straight flush returns correct score``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(6, Spade)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(8, Spade)
        let card5 = ValueCard(9, Spade)
        let hand = new Hand(card1, card2, card3, card4, card5)
        let expected = ([10],[7;6;5;4;3])

        let actual = hand.GetScore()

        actual |> should equal expected

    [<Test>]
    member x.``When comparing two pairs to a pair, two pairs should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(5, Heart)
        let card3 = ValueCard(6, Spade)
        let card4 = ValueCard(6, Diamond)
        let card5 = ValueCard(9, Spade)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = ValueCard(5, Spade)
        let card2' = ValueCard(5, Heart)
        let card3' = ValueCard(6, Spade)
        let card4' = ValueCard(7, Diamond)
        let card5' = ValueCard(9, Spade)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand1.GetScore() |> should be (greaterThan <| hand2.GetScore())

    [<Test>]
    member x.``When comparing two pairs to same two pairs, the one with higher card should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(5, Heart)
        let card3 = ValueCard(6, Spade)
        let card4 = ValueCard(6, Diamond)
        let card5 = ValueCard(9, Spade)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = ValueCard(5, Spade)
        let card2' = ValueCard(5, Heart)
        let card3' = ValueCard(6, Spade)
        let card4' = ValueCard(6, Diamond)
        let card5' = ValueCard(10, Spade)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand2.GetScore() |> should be (greaterThan <| hand1.GetScore())

    [<Test>]
    member x.``When comparing flush to straight, flush should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(6, Heart)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(8, Diamond)
        let card5 = ValueCard(9, Spade)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = ValueCard(2, Spade)
        let card2' = ValueCard(9, Spade)
        let card3' = ValueCard(6, Spade)
        let card4' = ValueCard(4, Spade)
        let card5' = ValueCard(10, Spade)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand2.GetScore() |> should be (greaterThan <| hand1.GetScore())

    [<Test>]
    member x.``When comparing two straights, the one with a higer card should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(6, Heart)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(8, Diamond)
        let card5 = ValueCard(9, Spade)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = ValueCard(10, Spade)
        let card2' = ValueCard(6, Heart)
        let card3' = ValueCard(7, Spade)
        let card4' = ValueCard(8, Diamond)
        let card5' = ValueCard(9, Spade)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand2.GetScore() |> should be (greaterThan <| hand1.GetScore())

    [<Test>]
    member x.``When comparing full house to flush, full house should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(5, Heart)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(7, Diamond)
        let card5 = ValueCard(7, Heart)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = ValueCard(2, Spade)
        let card2' = ValueCard(9, Spade)
        let card3' = ValueCard(6, Spade)
        let card4' = ValueCard(4, Spade)
        let card5' = ValueCard(10, Spade)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand1.GetScore() |> should be (greaterThan <| hand2.GetScore())

    [<Test>]
    member x.``When comparing full house to a higher full house, the one with bigger three should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(5, Heart)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(7, Diamond)
        let card5 = ValueCard(7, Heart)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = Ace(Spade)
        let card2' = Ace(Diamond)
        let card3' = ValueCard(6, Spade)
        let card4' = ValueCard(6, Diamond)
        let card5' = ValueCard(6, Heart)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand1.GetScore() |> should be (greaterThan <| hand2.GetScore())

    [<Test>]
    member x.``When comparing four of a kind to straight flush, straight flush should be greater``() =
        let card1 = ValueCard(5, Spade)
        let card2 = ValueCard(7, Club)
        let card3 = ValueCard(7, Spade)
        let card4 = ValueCard(7, Diamond)
        let card5 = ValueCard(7, Heart)
        let hand1 = new Hand(card1, card2, card3, card4, card5)

        let card1' = ValueCard(2, Spade)
        let card2' = ValueCard(3, Spade)
        let card3' = ValueCard(4, Spade)
        let card4' = ValueCard(5, Spade)
        let card5' = ValueCard(6, Spade)
        let hand2 = new Hand(card1', card2', card3', card4', card5')

        hand2.GetScore() |> should be (greaterThan <| hand1.GetScore())
end