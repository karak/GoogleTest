#light
module LastAndFirst.Ai

open LastAndFirst

type StubPlayer = class
  new() = {}
  interface IPlayer with
    member this.Respond move = Resign
  end
  end
  
let solve (words: string list) (initial: string) : IPlayer =
  new StubPlayer() :> IPlayer
