#light
module LastAndFirst.Question

open System.IO

let private readAllLines (filePath: string) = 
  seq {
    use reader = File.OpenText filePath
    while not reader.EndOfStream do
      yield reader.ReadLine()
  }

let loadFrom (filePath: string) : string * string list=
  let lines = List.ofSeq (readAllLines filePath)
  match lines with
  |initial :: skip :: wordList -> (initial, wordList)
  |_ -> raise (System.FormatException("valid format is: initial-word <eol> <empty> <eol> word1 <eol> word2 <eol> ..."))

//return initial, blackHistory, whiteHistory
let loadDialogFrom (dialogFilePath: string) : string * string list * string list =
  let addIndex xs = Seq.mapi (fun x y -> (x, y)) xs
  let removeIndex xs = Seq.map (fun (x, y) -> y) xs
  let isEven pair = match pair with |(x, _) -> x % 2 = 0
  let isOdd pair = not (isEven pair)

  let history = readAllLines dialogFilePath
  let indexedHistory = addIndex history
  let odds = List.ofSeq (removeIndex (Seq.filter isOdd indexedHistory))
  let evens = List.ofSeq (removeIndex (Seq.filter isEven indexedHistory))
  let initial = evens.Head
  let blackHistory = odds
  let whiteHistory = evens.Tail
  (initial, blackHistory, whiteHistory)
  
  
