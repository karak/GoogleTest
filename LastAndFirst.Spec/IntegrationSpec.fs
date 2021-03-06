#light
module LastAndFirst.Spec.IntegrationSpec

open LastAndFirst
open NaturalSpec

let rec mockRespond (enemy: IPlayer) (aiHistory: string list) (enemyHistory: string list) =
  if aiHistory = [] then
    fun x -> x
  else if enemyHistory = [] then
    Mock enemy.Respond aiHistory.Head Resign
  else
    let headFunc = Mock enemy.Respond aiHistory.Head (Answer enemyHistory.Head)
    let tailFunc = mockRespond enemy aiHistory.Tail enemyHistory.Tail
    fun x -> tailFunc (headFunc x)
    
let mockEnemyFromDialogFile (enemy: IPlayer) dialogFilePath=
  let _, blackHistory, whiteHistory = Question.loadDialogFrom dialogFilePath
  //printfn "black(AI): %A" blackHistory
  //printfn "white(Mock): %A" whiteHistory
  
  As enemy
    |> mockRespond enemy blackHistory whiteHistory


//avoid exception from NaturalSpec.mock
type IPlayerForMe = interface
  inherit IPlayer
  end

let enemy = mock<IPlayerForMe> "enemy"

[<ScenarioTemplate(@"../../../data/text1.txt", @"../../../data/text1_expected.txt")>]
[<ScenarioTemplate(@"../../../data/text2.txt", @"../../../data/text2_expected.txt")>]
let ``When actual question and recoreded dialog are given, it could replay it`` questionFilePath dialogFilePath =
  let initial, wordList = Question.loadFrom questionFilePath
  
  let ai = Ai.makeWinningPlayer wordList initial
  
  let whichWon player1 player2 =
    Game.winner initial player1 player2
  
  mockEnemyFromDialogFile enemy dialogFilePath
    |> When calculating whichWon ai
    |> It should equal ai
    |> Verify
