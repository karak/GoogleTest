#light 
module LastAndFirst.Game

let rec private winnerWithInitialMove initial (player1: IPlayer) (player2: IPlayer) =
    match initial with
    |Resign -> player1
    |_ -> winnerWithInitialMove (player1.Respond initial) player2 player1
  
let winner (player1: IBlackPlayer) (player2: IWhitePlayer) : IPlayer =
    winnerWithInitialMove (player1.Initiate ()) player2 player1
  