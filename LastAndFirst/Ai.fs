#light
module LastAndFirst.Ai

open LastAndFirst
open LastAndFirst.Construction
open LastAndFirst.Negamax

type StubPlayer = class
  new() = {}
  interface IPlayer with
    member this.Respond move = Resign
  end
  end

let solve (words: string list) (initial: string) : Score * string list =
  let tree = buildTree words initial 
  findBestway tree
  
let makeWinningPlayer (words: string list) (initial: string) : IPlayer =
  new StubPlayer() :> IPlayer
