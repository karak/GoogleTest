#light 

module NaturalSpec.Test.GameSpec

open System
open LastAndFirst
open LastAndFirst.Game
open NaturalSpec

  
//note: avoid multiple mocking of same type bacause it has caused ArgumentException!
let player1 = mock<IBlackPlayer> "player-1"
let player2 = mock<IWhitePlayer> "player-2"
let whichIsTheWinner () = winner player1 player2
let notResign = Answer "stub answer not to resign"


[<Scenario>]
let ``When player 1 is resigned at 1st move, player 2 should win``() =
  Given ()
    |> Mock player1.Initiate () Resign
    |> When calculating whichIsTheWinner
    |> It should equal (player2 :> IPlayer)
    |> Verify

[<Scenario>]
let ``When player 2 is resigned at 2nd move, player 1 should win``() =
  Given ()
    |> Mock player1.Initiate () notResign
    |> Mock player2.Respond notResign Resign
    |> When calculating whichIsTheWinner
    |> It should equal (player1 :> IPlayer)
    |> Verify

[<Scenario>]
let ``When player 1 is resigned at 3rd move, player 2 should win``() =
  Given ()
    |> Mock player1.Initiate () notResign
    |> Mock player2.Respond notResign notResign
    |> Mock player1.Respond notResign Resign
    |> When calculating whichIsTheWinner
    |> It should equal (player2 :> IPlayer)
    |> Verify

[<Scenario>]
let ``When player 2 is resigned at 4th move, player 1 should win``() =
  Given ()
    |> Mock player1.Initiate () notResign
    |> Mock player2.Respond notResign notResign
    |> Mock player1.Respond notResign notResign
    |> Mock player2.Respond notResign Resign
    |> When calculating whichIsTheWinner
    |> It should equal (player1 :> IPlayer)
    |> Verify
