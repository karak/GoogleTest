#light 
namespace LastAndFirst

type IPlayer = interface
  abstract member Respond: Move -> Move
  end

type IBlackPlayer = interface
  inherit IPlayer
  abstract member Initiate: unit -> Move
  end

type IWhitePlayer = interface
  inherit IPlayer
  end
