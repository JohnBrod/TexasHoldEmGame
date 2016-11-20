namespace Poker.TexasHoldEm

module Dealing = 

    open System

    type Suit = Hearts | Clubs | Spades | Diamonds
    type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    type Card = Rank * Suit
    type Stage = Hole | Flop | Turn | River
    type Cards = { flop : (Card * Card * Card); turn : Card; river : Card }

    let newDeck = 
        [(Two, Spades); (Three, Spades); (Four, Spades); (Five, Spades);
        (Six, Spades); (Seven, Spades); (Eight, Spades); (Nine, Spades);
        (Ten, Spades); (Jack, Spades); (Queen, Spades); (King, Spades); (Ace, Spades);
        (Two, Hearts); (Three, Hearts); (Four, Hearts); (Five, Hearts);
        (Six, Hearts); (Seven, Hearts); (Eight, Hearts); (Nine, Hearts);
        (Ten, Hearts); (Jack, Hearts); (Queen, Hearts); (King, Hearts); (Ace, Hearts);
        (Two, Clubs); (Three, Clubs); (Four, Clubs); (Five, Clubs);
        (Six, Clubs); (Seven, Clubs); (Eight, Clubs); (Nine, Clubs);
        (Ten, Clubs); (Jack, Clubs); (Queen, Clubs); (King, Clubs); (Ace, Clubs);
        (Two, Diamonds); (Three, Diamonds); (Four, Diamonds); (Five, Diamonds);
        (Six, Diamonds); (Seven, Diamonds); (Eight, Diamonds); (Nine, Diamonds);
        (Ten, Diamonds); (Jack, Diamonds); (Queen, Diamonds); (King, Diamonds); (Ace, Diamonds)]   

    let shuffle deck = 
        let random = new System.Random()
        deck |> List.sortBy (fun card -> random.Next())

    let deal players =
        let cards = newDeck |> shuffle |> Seq.take (5 + (players * 2)) |> Seq.toList
        let hole = cards |> List.take (players * 2) |> List.chunkBySize 2
        let flop = cards |> List.skip (players * 2) |> List.take 3 
        let turn = cards |> List.skip ((players * 2) + 3) |> List.take 1
        let river = cards |> List.skip ((players * 2) + 3 + 1) |> List.take 1
        let communityCards = {
            flop = (flop.[0], flop.[1], flop.[2])
            turn = turn.[0]
            river = river.[0]
        }
        (hole |> List.map (fun c -> (c.[0], c.[1])), communityCards, Hole)

    let nextStage stage = 
        match stage with
        | Hole -> Flop 
        | Flop -> Turn 
        | Turn -> River
