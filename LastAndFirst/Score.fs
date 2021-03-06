#light
namespace LastAndFirst

type Score =  Victory | Defeat with
  member this.Reversed
    with get (): Score =
      match this with
      | Victory -> Defeat
      | Defeat -> Victory
  end
