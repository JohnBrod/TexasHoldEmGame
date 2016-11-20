namespace Poker.TexasHoldEm

module TakingBets = 

    type Betting = { played : (int * int) list; playing : (int * int) list; minimumStake : int }

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
        | { played = pd; playing = (b, c)::[] } -> { played = []; playing = pd @ [(b + action, c - action)]; minimumStake = b + action }
        | { played = pd; playing = (b, c)::ps } -> { played = pd @ [(b + action, c - action)]; playing = ps; minimumStake = b + action }
        | { playing = [] } -> betting

    let raise action betting = 
        match betting with
        | { played = pd; playing = (b, c)::[] } -> { played = []; playing = pd @ [(b + action, c - action)]; minimumStake = b + action }
        | { played = pd; playing = (b, c)::ps } -> { played = pd @ [(b + action, c - action)]; playing = ps; minimumStake = b + action }
        | { playing = [] } -> betting

    let (|Fold|Check|Call|Raise|) (action, { playing = (b, c)::_; minimumStake = ms }) = 
        if action + b < ms then Fold else if action = 0 then Check else if action + b = ms then Call else Raise

    let finished bets = 
        match bets with
        | { playing = []; minimumStake = 0 } -> true   // all check
        | { played = []; playing = p::[]; minimumStake = 0 } -> true  // all fold
        | { playing = (b, c)::ps; minimumStake = ms } -> ms = b && b > 0 // all paid
        | _ -> false

    let play action betting = 
        match action, betting with 
        | Fold -> fold betting
        | Check -> check betting
        | Call -> call action betting
        | Raise -> raise action betting

    let restartBetting { played = pd; playing = ps; } =
        { played = []; playing = pd @ ps; minimumStake = 0 }

    let startBetting numPlayers chips =
        { played = []; playing = List.init numPlayers (fun _ -> (0, chips)); minimumStake = 0 }
