module PE54

open System

type Suit = 
    | Heart
    | Diamond
    | Spade
    | Club
    override this.ToString() =
        match this with
        | Heart -> "Heart"
        | Diamond -> "Diamond"
        | Spade -> "Spade"
        | Club -> "Club"

type Card =
    | Ace of Suit
    | King of Suit
    | Queen of Suit
    | Jack of Suit
    | ValueCard of int * Suit

    member this.Value = 
        match this with
        | Ace(_) -> 14
        | King(_) -> 13
        | Queen(_) -> 12
        | Jack(_) -> 11
        | ValueCard(x,_) -> x

    member this.Suit = 
        match this with
        | Ace(s) -> s
        | King(s) -> s
        | Queen(s) -> s
        | Jack(s) -> s
        | ValueCard(_,s) -> s

    override this.ToString() =
        match this with
        | Ace(s) -> sprintf "Ace of %s" <| s.ToString()
        | King(s) -> sprintf "King of %s" <| s.ToString()
        | Queen(s) -> sprintf "Queen of %s" <| s.ToString()
        | Jack(s) -> sprintf "Jack of %s" <| s.ToString()
        | ValueCard(x,s) -> sprintf "%d of %s" x <| s.ToString()
        
type PokerHand =
    | HighCard of int
    | OnePair of int
    | TwoPair of int * int
    | ThreeOfAKind of int
    | Straight of int //Highest card's value
    | Flush
    | FullHouse of int * int
    | FourOfAKind of int
    | StraightFlush of int //Highest card's value

let getSuitFromCard (cardAsString : string) =
    if String.IsNullOrWhiteSpace(cardAsString) then
        raise <| new System.ArgumentException("cardAsString")
    let tmp = cardAsString.ToCharArray()
    let suitChar = Char.ToLowerInvariant(tmp.[(Array.length tmp) - 1])
    match suitChar with
    | 's' -> Spade
    | 'c' -> Club
    | 'h' -> Heart
    | 'd' -> Diamond
    | x -> failwithf "Not a valid suit '%c'" x

let stringToCard (cardAsString : string) = 
    let suit = getSuitFromCard cardAsString
    let withoutSuit = cardAsString.Substring(0, cardAsString.Length - 1)
    let (success, number) = Int32.TryParse(withoutSuit)
    if success then
        if number < 2 || number > 9 then
            failwithf "Card has to be between 2 and 9 but given %d" number
        ValueCard(number, suit)
    else
        match Char.ToLower(withoutSuit.[0]) with
        | 'a' -> Ace(suit)
        | 'k' -> King(suit)
        | 'q' -> Queen(suit)
        | 'j' -> Jack(suit)
        | 't' -> ValueCard(10, suit)
        | x -> failwithf "Not a valid card '%s'" cardAsString

type Hand(card1 : Card, card2 : Card, card3 : Card, card4 : Card, card5 : Card) =
    let cards = [card1; card2; card3; card4; card5]

    member this.Cards = cards

    new(text : string) =
        let split = text.Split([| ' ' |])
        if Array.length split <> 5 then
            raise <| new ArgumentException("text")
        new Hand(stringToCard split.[0], stringToCard split.[1], stringToCard split.[2], 
            stringToCard split.[3], stringToCard split.[4])

    member this.GetScore() =
        let ranks = Array.create 13 0
        let cards = this.Cards
        cards |> List.iter (fun c -> ranks.[c.Value - 2] <- ranks.[c.Value - 2] + 1)
        let possibleScore = 
            [|0..12|] 
            |> Array.zip ranks
            |> Array.filter (fun (cnt, _) -> cnt > 0) 
            |> Array.toList 
            |> List.sort
            |> List.rev
            |> List.unzip
        if List.length <| fst possibleScore <> 5 then
            possibleScore
        else
            // High card, straight, flush or straight flush
            let suit = (List.head cards).Suit
            let isFlush = cards |> List.exists (fun c -> c.Suit <> suit) |> not
            let cardValues = match snd possibleScore with 12 :: 3 :: _ -> [3;2;1;0;-1] | x -> x // Straight from Ace
            let isStraight = (List.head <| cardValues) - (cardValues |> List.rev |> List.head) = 4
            match isStraight, isFlush with
            | true, false -> ([3;1;2], cardValues)
            | false, true -> ([3;1;3], cardValues)
            | true, true -> ([10], cardValues)
            | _, _ -> possibleScore

    member this.GetPokerHand() =
        let cards = this.Cards
        let score = this.GetScore()
        match fst score with
        | [1;1;1;1;1] -> HighCard(List.head <| snd score)
        | [2;1;1;1] -> OnePair(List.head <| snd score)
        | [3;1;1] -> ThreeOfAKind(List.head <| snd score)
        | [3;1;2] -> Straight(List.head <| snd score)
        | [3;1;3] -> Flush
        | [3;2] -> FullHouse(List.head <| snd score, snd score |> List.rev |> List.head)
        | [4;1] -> FourOfAKind(List.head <| snd score)
        | [10] -> StraightFlush(List.head <| snd score)
        | x -> failwithf "Unknown score '%A' for a hand %A" x cards

