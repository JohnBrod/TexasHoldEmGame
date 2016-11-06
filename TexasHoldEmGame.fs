open System

type Suit = Hearts | Clubs | Spades | Diamonds
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = Rank * Suit
type DealStage = Hole | Flop | Turn | River
type Cards = { hole : (Card * Card) list; flop : (Card * Card * Card); turn : Card; river : Card; Stage : DealStage }
type Betting = { played : int list; playing : int list; minimumStake : int }
type Game = { cards : Cards; betting : Betting }

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
    {
        hole = hole |> List.map (fun c -> (c.[0], c.[1]))
        flop = (flop.[0], flop.[1], flop.[2])
        turn = turn.[0]
        river = river.[0]
        Stage = Hole
    }

let fold betting = 
    match betting with
    | { played = pd; playing = p::[] } -> { betting with played = []; playing = pd }
    | { played = pd; playing = p::ps } -> { betting with played = pd; playing = ps }
    | { playing = [] } -> betting 

let check betting = 
    match betting with
    | { played = pd; playing = p::[] } -> { betting with played = pd @ [p]; playing = [] }
    | { played = pd; playing = p::ps } -> { betting with played = pd @ [p]; playing = ps }
    | { playing = [] } -> betting

let call action betting = 
    match betting with
    | { played = pd; playing = p::[] } -> { played = []; playing = pd @ [p + action]; minimumStake = p + action }
    | { played = pd; playing = p::ps } -> { played = pd @ [p + action]; playing = ps; minimumStake = p + action }
    | { playing = [] } -> betting

let raise action betting = 
    match betting with
    | { played = pd; playing = p::[] } -> { played = []; playing = pd @ [p + action]; minimumStake = p + action }
    | { played = pd; playing = p::ps } -> { played = pd @ [p + action]; playing = ps; minimumStake = p + action }
    | { playing = [] } -> betting

let (|Fold|Check|Call|Raise|) (action, { playing = stake::_; minimumStake = ms }) = 
    if action + stake < ms then Fold else if action = 0 then Check else if action + stake = ms then Call else Raise

let placing bets = 
    match bets with
    | { playing = []; minimumStake = 0 } -> false   // all check
    | { played = []; playing = p::[]; minimumStake = 0 } -> false  // all fold
    | { playing = p::ps; minimumStake = ms } -> ms <> p  // all paid
    | _ -> true
    // big blind option???

let play action betting = 
    match action, betting with 
    | Fold -> fold betting
    | Check -> check betting
    | Call -> call action betting
    | Raise -> raise action betting

let startBetting { played = pd; playing = ps; } =
    { played = []; playing = pd @ ps; minimumStake = 0 }

let message action betting = 
    match action, betting with 
    | Fold -> "fold"
    | Check -> "check"
    | Call -> "call " + string action
    | Raise -> "raise "  + string action

let dealing { Stage = s } = s < River

let nextStage cards = 
    match cards with
    | { hole = h; flop = f; turn = t; river = r; Stage = Hole } -> { hole = h; flop = f; turn = t; river = r; Stage = Flop }
    | { hole = h; flop = f; turn = t; river = r; Stage = Flop } -> { hole = h; flop = f; turn = t; river = r; Stage = Turn }
    | { hole = h; flop = f; turn = t; river = r; Stage = Turn } -> { hole = h; flop = f; turn = t; river = r; Stage = River }

[<EntryPoint>]
let main args =

    let mutable bets = { played = [10;20]; playing = [0;0]; minimumStake = 20 }

    let mutable cards = deal 4
    
    while dealing cards do

        printfn "%A" cards
        while placing bets do

            printfn "%A" bets
            let action = Int32.Parse(Console.ReadLine())
            printfn "%A" (message action bets)            
            bets <- play action bets

        cards <- nextStage cards
        bets <- startBetting bets

    0