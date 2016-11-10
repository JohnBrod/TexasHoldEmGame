open System
open Poker.TexasHoldEm.Dealing
open Poker.TexasHoldEm.TakingBets

type Player = { name : string; chips : int; stake : int; cards : (Card * Card) option }
type Game = { Players : Player list; CommunityCards : Cards }

let message action betting = 
    match action, betting with 
    | Fold -> "fold"
    | Check -> "check"
    | Call -> "call " + string action
    | Raise -> "raise "  + string action

let finished ps = ps |> List.map (fun x -> x.chips) |> List.max = 4000

let action = Int32.Parse(Console.ReadLine())

let rec gameLoop game = 
    printfn "%A" game
    if finished game.Players 
    then printfn "done"
    else gameLoop { game with Players = play action game.Players }

let setup players = players 
                    |> Array.toList
                    |> List.map (fun n -> { name = n; chips = 1000; stake = 0; cards = None })

[<EntryPoint>]
let main players =
    gameLoop { Players = setup players; CommunityCards = deal (Array.length players) }
    0

// todo: 
// player names
// player chips
// link players to cards
// blinds
// card comparison
// divvy up winnings
// while playing tournament
// big blind option
