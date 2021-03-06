#light 
module LastAndFirst.Assistant

open System
open LastAndFirst.Ai
open LastAndFirst.Game

type HumanPlayer() = class
  interface IPlayer with
    member this.Respond (answer: string) =
      let requestAnswer () =
        printfn "answer: %A" answer
        printfn "> select word(input empty if you'd resign)"
        let inputString = Console.ReadLine()
        if inputString.Length = 0 then Resign
        else Answer inputString
      let firstAnswer = requestAnswer()
      match firstAnswer with
      |Resign ->
        printfn "> Are you sure to resign?"
        requestAnswer ()
      |_ -> firstAnswer
      
      //TODO: validate inputString is permit
  end
  end

let initial, wordList = Question.loadFrom @"../../../data/text2.txt"

let ai = makeWinningPlayer wordList initial
let enemy = HumanPlayer() :> IPlayer

printfn "** last and first game **"
printfn "  word list: %A" wordList
printfn "  initial: %A" initial
let whoWon = winner initial ai enemy

if whoWon = ai then
  printfn "** AI win! **"
else
  printfn "** AI lose! **"
