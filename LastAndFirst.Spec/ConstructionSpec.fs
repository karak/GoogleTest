#light
module LastAndFirst.Spec.ConstructionSpec

open LastAndFirst.Negamax
open LastAndFirst.Construction
open NaturalSpec

let private makeLeaf x = Node (x, Seq.empty)
let rec private makeLinearTree (words: string list) =
  let children = if words.Tail = [] then Seq.empty else seq { yield makeLinearTree words.Tail }
  Node (words.Head, children)

[<Scenario>]
let ``when word-list is singular, it shoudl be root-only``() =
  Given "a"
    |> When calculating buildTree ["a"]
    |> It should equal (makeLeaf "a")
    |> Verify


[<Scenario>]
let ``when 2 words can follow the initial word, it should be binary-tree``() =
  Given "ab"
    |> When calculating buildTree ["ab" ; "bc" ; "bd" ]
    |> It should equal (Node ("ab", seq { yield (makeLeaf "bc") ; yield (makeLeaf "bd") }))
    |> Verify

[<Scenario>]
let ``when word-list is cyclic, it shoudl be linear``() =
  let cyclic = [ "ca" ; "bc" ; "ab" ]
  Given "ab"
    |> When calculating buildTree cyclic
    |> It should equal (makeLinearTree ["ab" ; "bc" ; "ca" ])
    |> Verify
