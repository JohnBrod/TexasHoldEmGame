namespace Poker.TexasHoldEm

module Game = 

    open System
    open Poker.TexasHoldEm.Dealing
    open Poker.TexasHoldEm.TakingBets

    type Game = { players : Betting; holeCards : (Card * Card) list; communityCards : Cards; stage : Stage; messages : string list }

    let tournamentOver cs = cs |> List.max = 4000

    let message players = 
        match players with
        | { played = pd; playing = (n,_,_)::rest; minimumStake = ms } -> [n + " to bet, min " + string(ms)]
        | _ -> ["error"]

    let handMessage players = 
        match players with
        | { played = (n,_,_)::rest } -> [n + " wins the hand"; n + " to start betting"]
        | _ -> ["error"]

    let startHand players = 
        let (hole, community, stage) = deal (List.length players.playing)
        { 
            players = restartBetting players;
            holeCards = hole; 
            communityCards = community;
            stage = stage;
            messages = message players
        }

    let startGame players = 
        startHand (startBetting players 1000)

    let next game action = 

        let players = play action game.players

        if tournamentOver ((game.players.played @ game.players.playing) |> List.map (fun (_,_,c) -> c))
        then { game with messages = ["winner is ..."] }
        else if game.stage < River && finished players
        then { game with players = restartBetting players; stage = nextStage game.stage; messages = message players }
        else if game.stage = River && finished players
        then { startHand game.players with messages = handMessage players }
        else { game with players = players; messages = message players }


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
// fold
