open System
open Poker.TexasHoldEm.Dealing
open Poker.TexasHoldEm.TakingBets

let message action betting = 
    match action, betting with 
    | Fold -> "fold"
    | Check -> "check"
    | Call -> "call " + string action
    | Raise -> "raise "  + string action


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