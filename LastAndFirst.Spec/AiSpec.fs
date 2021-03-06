#light
module LastAndFirst.Spec.AiSpec

open LastAndFirst
open LastAndFirst.Ai
open NaturalSpec


let resignAtOnce (player: IPlayer) =
  let responceResign(initial: string) =
    (player.Respond initial) = Resign
  be responceResign

let listAnswers words (inputAnswers: string list) =
  let rec dialog (player: IPlayer) (inputAnswers: string list) : string list  =
    if inputAnswers.IsEmpty then
      []
    else
      let responce = player.Respond inputAnswers.Head
      match responce with
      | Resign -> []
      | Answer answer -> answer :: (dialog player inputAnswers.Tail)
  let initialAnswer = inputAnswers.Head
  let player = makeWinningPlayer words initialAnswer
  dialog player inputAnswers

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

[<Scenario>]
let ``When 2 serial words are given, it should answer to win``() =
  Given ["ab"]
  |> When calculating listAnswers ["bc" ; "ab"]
  |> It should equal ["bc"]
  |> Verify

[<Scenario>]
let ``When both conclusion exist, it should choose answer to win``() =
  Given ["ab"]
  |> When calculating listAnswers ["bc" ; "ab"; "bd" ; "ce" ]
  |> It should equal ["bd"]
  |> Verify

[<Scenario>]
let ``When the best way is long, it should answer many times to win``() =
  Given ["ab" ; "cd"]
  |> When calculating listAnswers ["bc"; "cd" ; "ab" ; "de" ]
  |> It should equal ["bc" ; "de"]
  |> Verify

[<Scenario>]
let ``When enemy missed the best way, find local best way again``() =
  Given ["ab" ; "ce"]
  |> When calculating listAnswers ["ab"; "bc" ; "cd" ; "ce" ; "ef" ]
  |> It should equal ["bc" ; "ef"]
  |> Verify

[<Scenario>]
let ``When enemy missed the best way twice, find local best way again and again``() =
  Given ["ab" ; "ce"; "fh"]
  |> When calculating listAnswers ["ab"; "bc" ; "cd" ; "ce" ; "ef"; "fg" ; "fh"; "hi" ]
  |> It should equal ["bc" ; "ef"; "hi"]
  |> Verify
