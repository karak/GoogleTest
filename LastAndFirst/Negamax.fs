#light
module LastAndFirst.Negamax

[<CustomEquality>]
[<NoComparison>]
type Tree = Node of string * seq<Tree> with
  override this.Equals obj =
    match obj with
    | :? Tree as other ->
      match other with
        Node (otherAnswer, otherChildren) ->
        match this with
        | Node (answer, children) ->
          answer = otherAnswer && [for x in children -> x] = [for x in otherChildren -> x]
          //NOTE: convert to list because sequence doesn't not compare by element
    | _ -> false
    
  override this.GetHashCode () =
    match this with
    | Node (answer, _) -> answer.GetHashCode()
    //ignore sequence
  

///最善の評価値とその際の履歴を求める
let rec findBestway (tree: Tree) : Score * string list =
   match tree with
   | Node (myAnswer, childrenSeq) ->
     let children = [for x in childrenSeq -> x]
     if children.IsEmpty then
       (Defeat, [myAnswer])
     else
       let (score:Score), tail = findWorstWay children
       (score.Reversed, myAnswer :: tail)
and private findWorstWay (trees: Tree list)=
 let next = trees.Head //assert trees is not empty
 let nextScore, nextHistory = findBestway next
 if nextScore = Defeat || trees.Tail.IsEmpty then
   (nextScore, nextHistory)
 else
   findWorstWay trees.Tail