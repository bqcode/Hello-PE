module PE54

type Suit = 
    | Heart
    | Diamond
    | Spade
    | Club

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


let deckOfCards =
    [
        for suit in [ Spade; Club; Heart; Diamond ] do
            yield Ace(suit)
            yield King(suit)
            yield Queen(suit)
            yield Jack(suit)
            for value in 10 .. -1 .. 2 do
                yield ValueCard(value, suit)
    ]