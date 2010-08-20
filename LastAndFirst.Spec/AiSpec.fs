#light
module LastAndFirst.Spec.AiSpec

open LastAndFirst
open LastAndFirst.Ai
open NaturalSpec

let resignAtOnce (player: IPlayer) =
  let responceResign(initial: string) =
    (player.Respond (Answer initial)) = Resign
  be responceResign

[<Scenario>]
let ``When start with one word, it should resign``() =
  Given "a"
  |> When calculating solve ["a"]
  |> It should resignAtOnce
  |> Verify

[<Scenario>]
let ``When start with one word actually, it should resign``() =
  Given "ab"
  |> When calculating solve ["ab" ; "cd"]
  |> It should resignAtOnce
  |> Verify
