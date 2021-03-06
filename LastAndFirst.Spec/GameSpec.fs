#light 

module LastAndFirst.Spec.GameSpec

open System
open LastAndFirst
open LastAndFirst.Game
open NaturalSpec

//avoid error caused in NaturapSpec mock
type IPlayerDiff = interface 
  inherit IPlayer
end
  
//note: avoid multiple mocking of same type bacause it has caused ArgumentException!
let player1 = mock<IPlayer> "player-1"
let player2 = (mock<IPlayerDiff> "player-2") :> IPlayer
let answer = "stub answer not to resign"
let notResign = Answer answer
let whichIsTheWinner () = winner answer player1 player2


[<Scenario>]
let ``When player 1 is resigned at 1st move, player 2 should win``() =
  Given ()
    |> Mock player1.Respond answer Resign
    |> When calculating whichIsTheWinner
    |> It should equal player2
    |> Verify

[<Scenario>]
let ``When player 2 is resigned at 2nd move, player 1 should win``() =
  Given ()
    |> Mock player1.Respond answer notResign
    |> Mock player2.Respond answer Resign
    |> When calculating whichIsTheWinner
    |> It should equal player1
    |> Verify

[<Scenario>]
let ``When player 1 is resigned at 3rd move, player 2 should win``() =
  Given ()
    |> Mock player1.Respond answer notResign
    |> Mock player2.Respond answer notResign
    |> Mock player1.Respond answer Resign
    |> When calculating whichIsTheWinner
    |> It should equal player2
    |> Verify
