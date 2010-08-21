#light
module LastAndFirst.Spec.AiSpec

open LastAndFirst
open LastAndFirst.Ai
open NaturalSpec

[<Scenario>]
let ``When 2 serial words are given, it should answer``() =
  Given "ab"
  |> When calculating solve ["bc" ; "ab"]
  |> It should equal (Victory, ["ab" ; "bc"])
  |> Verify

let resignAtOnce (player: IPlayer) =
  let responceResign(initial: string) =
    (player.Respond (Answer initial)) = Resign
  be responceResign

[<Scenario>]
let ``When start with one word, it should resign``() =
  Given "a"
  |> When calculating makeWinningPlayer ["a"]
  |> It should resignAtOnce
  |> Verify

[<Scenario>]
let ``When start with one word actually, it should resign``() =
  Given "ab"
  |> When calculating makeWinningPlayer ["ab" ; "cd"]
  |> It should resignAtOnce
  |> Verify

