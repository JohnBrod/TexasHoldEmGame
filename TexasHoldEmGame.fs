namespace Poker.TexasHoldEm

module Game = 

    open System
    open Poker.TexasHoldEm.Dealing
    open Poker.TexasHoldEm.TakingBets

    type Game = { players : string list; bets : Betting; holeCards : (Card * Card) list; communityCards : Cards; stage : Stage; messages : string list }

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
            stage = stage;
            messages = ["blinds etc, xxx to bet"]
        }

    let next game action = 

        let bets = play action game.bets

        if tournamentOver ((game.bets.played @ game.bets.playing) |> List.map (fun (_,c) -> c))
        then { game with messages = ["winner is ..."] }
        else if game.stage < River && finished bets
        then { game with bets = restartBetting bets; stage = nextStage game.stage; messages = ["xx to bet"] }
        else if game.stage = River && finished bets
        then { startGame game.players with messages = ["xx wins hand";"xx to bet"] }
        else { game with bets = bets; messages = ["xx to bet"] }


// todo: 
// end of hand
//      create hands
//      hand comparison
//          compare player hands to get best for player
//          compare best to other players
//      winner
//      divvy pot
// blinds
//      big blind option
// split game into another file
//      return messages with the next play (e.g. bet john, min 100; )
