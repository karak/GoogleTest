#light
module LastAndFirst.Negamax

type Score =  Victory | Defeat
let reverse score =
   match score with
    | Victory -> Defeat
    | Defeat -> Victory

type Tree = Node of string * seq<Tree>

///最善の評価値とその際の履歴を求める
let rec findBestway (tree: Tree) : Score * string list =
   match tree with
   | Node (myAnswer, childrenSeq) ->
     let children = [for x in childrenSeq -> x]
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