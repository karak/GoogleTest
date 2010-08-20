#light
module LastAndFirst.Spec.NegamaxSpec

open LastAndFirst
open LastAndFirst.Negamax
open NaturalSpec

let leaf = Node("0", [])

[<Scenario>]
let ``When it is leaf, defeat at 1st``()  =
  Given leaf
    |> When calculating negamax
    |> It should equal (Defeat, ["0"])
    |> Verify

let whoHasLeafOnly = Node ("1", [leaf])

[<Scenario>]
let ``When its child is a leaf, victory at 2nd``()  =
  Given whoHasLeafOnly
    |> When calculating negamax
    |> It should equal (Victory, ["1" ; "0"])
    |> Verify

let whoHasLeaf = Node ("1", [leaf ; whoHasLeafOnly ; whoHasLeafOnly])

[<Scenario>]
let ``When any of its children is a leaf, victory at 2nd``()  =
  Given whoHasLeaf
    |> When calculating negamax
    |> It should equal (Victory, ["1" ; "0"])
    |> Verify

let whoseNextHasLeafOnly = Node ("2", [whoHasLeafOnly ; whoHasLeafOnly ; whoHasLeaf])

[<Scenario>]
let ``When any of its children has a leaf, defeat at 3rd``()  =
  Given whoseNextHasLeafOnly
    |> When calculating negamax
    |> It should equal (Defeat, ["2" ; "1" ; "0"])
    |> Verify
