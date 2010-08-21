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
    
  member this.Count
    with get() : int =
      this._data.Count
  end

type internal WinningPlayer = class
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

let solve (words: string list) (initial: string) : Score * string list =
  let tree = buildTree words initial 
  findBestway tree
  
let makeWinningPlayer (words: string list) (initial: string) : IPlayer =
  let score, bestWay = (solve words initial)
  new WinningPlayer(bestWay) :> IPlayer
