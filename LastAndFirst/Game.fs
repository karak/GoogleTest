#light 
module LastAndFirst.Game

let rec private winnerWithPreviousMove initial (player1: IPlayer) (player2: IPlayer) =
    match initial with
    |Resign -> player1
    |_ -> winnerWithPreviousMove (player1.Respond initial) player2 player1
  
let winner (player1: IBlackPlayer) (player2: IWhitePlayer) : IPlayer =
    winnerWithPreviousMove (player1.Initiate ()) player2 player1
  