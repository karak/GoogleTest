#light
module LastAndFirst.Ai

open System
open LastAndFirst
open LastAndFirst.Construction
open LastAndFirst.Negamax

type Queue<'T> = class
  val _data: ResizeArray<'T>
  
  new (data: seq<'T>) as this =
    {_data = new ResizeArray<'T>(data)} then
    this._data.Reverse()
  
  member this.Dequeue () : 'T =
    let lastIndex = this._data.Count - 1
    let last = this._data.[lastIndex]
    this._data.RemoveAt lastIndex
    last
  
  //not enqueue, but push the original item in order to restore queue
  member this.Requeue item =
    this._data.Insert(this._data.Count, item)
    
  member this.Count
    with get() : int =
      this._data.Count
  end

type internal StaticWinningPlayer = class
  val private _bestWay: Queue<string>
  
  new(bestWay: seq<string>) =
    { _bestWay = new Queue<string>(bestWay) }
  
  interface IPlayer with
    member this.Respond answer =
      if this._bestWay.Count = 0 then
        raise (InvalidOperationException("never expecting to be required to respond"))
      else
        let expected = this._bestWay.Dequeue()
        if answer <> expected then
          this._bestWay.Requeue expected
          let message = sprintf "different from best way actual:%s, expected:%s" answer expected
          raise (ArgumentException(message))
        else
          if this._bestWay.Count = 0 then
            Resign
          else
            let myAnswer = this._bestWay.Dequeue()
            Answer myAnswer
  end
  end


let private solve (words: string list) (initial: string) : string list =
  let tree = buildTree words initial 
  let score, bestway = findBestway tree
  bestway
  
type internal DynamicWinningPlayer = class
  val mutable private _innerPlayer: IPlayer
  val private _usedWords: ResizeArray<string>
  val private _words: string list
  val private _initial: string
  
  new (words: string list, initial: string) =
    {
      _innerPlayer = new StaticWinningPlayer(solve words initial)
      _usedWords = new ResizeArray<string>()
      _words = words
      _initial = initial
    }

  interface IPlayer with
    override this.Respond answer =
      let responce = this.DoRespond answer
      this.MarkUsed answer
      match responce with
      |Answer myAnswer -> this.MarkUsed myAnswer
      |Resign -> ()
      responce
  end
  member private this.DoRespond answer = 
    try
      this._innerPlayer.Respond answer
    with
    | :? System.ArgumentException ->
      this.Reconstruct answer
      this._innerPlayer.Respond answer
    | ex -> raise ex

  member private this.MarkUsed word =
    this._usedWords.Insert(this._usedWords.Count, word)  
  
  member private this.Reconstruct newInitial =
    let isUnused x = this._usedWords.BinarySearch(x) < 0
    this._usedWords.Sort()
    let newWords = List.filter isUnused this._words
    
    this._innerPlayer <- new StaticWinningPlayer(solve newWords newInitial)
  end
  
  
let makeWinningPlayer (words: string list) (initial: string) : IPlayer =
  new DynamicWinningPlayer(words, initial) :> IPlayer
