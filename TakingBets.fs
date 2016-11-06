namespace Poker.TexasHoldEm

module TakingBets = 

    type Betting = { played : int list; playing : int list; minimumStake : int }

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
