fsc -a TakingBets.fs
fsc -a Dealing.fs
fsc -a TexasHoldEmGame.fs -r TakingBets.dll -r Dealing.dll 
fsc PokerConsole.fs -r TexasHoldEmGame.dll