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
let rec negamax (tree: Tree) : Score * string list =
   match tree with
   | Node (myAnswer, children) ->
     let length = children.Length
     if length = 0 then
       (Defeat, [myAnswer])
     else
       let next = children.Head
       let (nextScore, nextHistory) = negamax next
       if nextScore = Defeat then
         (Victory, myAnswer :: nextHistory)
       else if length = 1 then
         ((reverse nextScore), myAnswer :: nextHistory)
       else  //本当に子どもがいない場合と子どもの中にDefeatがない場合を区別する必要がある
         negamax (Node(myAnswer, children.Tail))