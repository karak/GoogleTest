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


