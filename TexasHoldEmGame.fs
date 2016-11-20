open System
open Poker.TexasHoldEm.Dealing
open Poker.TexasHoldEm.TakingBets

type Game = { players : string list; bets : Betting; holeCards : (Card * Card) list; communityCards : Cards; stage : Stage }

let tournamentOver cs = cs |> List.max = 4000

let setup players = players 
                    |> Array.toList

let startGame players = 
    let (hole, community, stage) = deal (List.length players)
    { 
        players = players; 
        bets = startBetting (List.length players) 1000;
        holeCards = hole; 
        communityCards = community;
        stage = stage
    }

let rec gameLoop game = 
    printfn "%A" game
    let action = Int32.Parse(Console.ReadLine())
    let bets = play action game.bets

    if tournamentOver ((game.bets.played @ game.bets.playing) |> List.map (fun (_,c) -> c))
    then printfn "announce winner"
    else if game.stage = River && finished bets
    then printfn "xx wins hand"
    else printfn "xx to bet"

    if game.stage < River && finished bets
    then gameLoop { game with bets = restartBetting bets; stage = nextStage game.stage }
    else if game.stage = River && finished bets
    then gameLoop (startGame game.players)
    else gameLoop { game with bets = bets }

[<EntryPoint>]
let main players =
    gameLoop (startGame (setup players))
    0

// todo: 
// betting reduces chips
// end of hand
//      winner
//      divvy pot
// link players to cards
// blinds
// card comparison
// big blind option
// 