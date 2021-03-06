#light
module LastAndFirst.Construction

open LastAndFirst.Negamax

let rec buildTree (words: string list) (prevWord: string): Tree =
  let wordsButPrev = List.filter (fun x -> x <> prevWord) words
  let children = 
    if words = [] then
     Seq.empty
    else
      //TODO: assert words.Contains prevWord
      let firstOf (x: string) = x.[0]
      let lastOf (x: string) = x.[x.Length - 1]
      let canFollow x = ((firstOf x) = (lastOf prevWord))
      let followableWords = List.filter canFollow wordsButPrev
      seq { for w in followableWords -> buildTree wordsButPrev w }
  Node (prevWord, children)
 
