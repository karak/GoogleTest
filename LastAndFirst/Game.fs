#light 
module LastAndFirst.Game

let rec private winnerWithPreviousMove initial (player1: IPlayer) (player2: IPlayer) =
    match initial with
    |Resign -> player1
    |Answer answer -> winnerWithPreviousMove (player1.Respond answer) player2 player1
  
let winner initial (player1: IPlayer) (player2: IPlayer) : IPlayer =
    winnerWithPreviousMove (Answer initial) player1 player2
  