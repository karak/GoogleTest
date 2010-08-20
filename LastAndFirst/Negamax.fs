#light
module LastAndFirst.Negamax

type Score =  Victory | Defeat
let reverse score =
   match score with
    | Victory -> Defeat
    | Defeat -> Victory

///TODO: 巨大な木に対応するためには、遅延評価を実現できる必要がある
type Tree = Node of string * Tree list

///最善の評価値とその際の履歴を求める
let rec findBestway (tree: Tree) : Score * string list =
   match tree with
   | Node (myAnswer, children) ->
     if children.IsEmpty then
       (Defeat, [myAnswer])
     else
       let score, tail = findWorstWay children
       (reverse score, myAnswer :: tail)
and private findWorstWay (trees: Tree list)=
 let next = trees.Head //assert trees is not empty
 let nextScore, nextHistory = findBestway next
 if nextScore = Defeat || trees.Tail.IsEmpty then
   (nextScore, nextHistory)
 else
   findWorstWay trees.Tail