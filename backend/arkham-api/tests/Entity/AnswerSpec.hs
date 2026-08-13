module Entity.AnswerSpec (spec) where

import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep qualified as CS
import Arkham.Classes.HasGame (getGame)
import Arkham.Difficulty
import Data.UUID (fromWords64)
import Entity.Answer
import TestImport.New

{- | A second seat at the table. 'handleAnswerPure' only looks players up in
@gameQuestion@, so it does not need a matching investigator.
-}
otherPlayer :: PlayerId
otherPlayer = PlayerId (fromWords64 0 2)

afterSkillTestSeat :: [UI Message] -> Question Message
afterSkillTestSeat = QuestionLabel "$label.chooseAfterSkillTestEffect" Nothing . ChooseOneAtATime

answerFirstChoice :: PlayerId -> Answer
answerFirstChoice pid =
  Answer QuestionResponse {qrChoice = 0, qrPlayerId = Just pid, qrQuestionVersion = Nothing}

reparked :: [Message] -> Map PlayerId (Question Message)
reparked msgs = mconcat [m | Retain (AskMap m) <- msgs]

spec :: Spec
spec = do
  describe "CampaignStepAnswer" do
    it "rejects a stale campaign answer after a side scenario has started" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let
        nextStep = CS.ScenarioStep "51025"
        continuation =
          CS.ContinueCampaignStep
            $ CS.Continuation nextStep True False Nothing False
        sideScenario = CS.StandaloneScenarioStep "81001" continuation
        campaign =
          overAttrs
            (\a -> a {campaignStep = sideScenario})
            (lookupCampaign "51" Easy)

      overTest $ \g ->
        g
          { gameMode =
              These
                campaign
                (fromJustNote "test harness always has a scenario" $ modeScenario g.gameMode)
          , gameQuestion =
              singletonMap pid
                $ QuestionLabel "$chooseLeadInvestigator" Nothing (ChooseOne [])
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (CampaignStepAnswer sideScenario)) >>= \case
        Unhandled _ -> pure ()
        Handled messages ->
          expectationFailure
            $ "stale campaign answer was accepted: "
            <> show messages

  -- #4787: an after-skill-test AskMap is built from messages already popped off
  -- the queue, so nothing regenerates the seats it publishes. Before Retain, one
  -- player answering discarded every other player's option -- Unrelenting (1)'s
  -- three UnsealChaosToken messages among them, permanently shrinking the bag.
  describe "retained questions" do
    it "keeps the other seats parked when one seat answers" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let theirs = afterSkillTestSeat [Label "Unrelenting (1)" [ClearUI]]
      overTest $ \g ->
        g
          { gameQuestion =
              mapFromList
                [ (pid, afterSkillTestSeat [Label "Quick Thinking" [ClearUI]])
                , (otherPlayer, theirs)
                ]
          , gameRetainedQuestion = True
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled msgs -> do
          Run [ClearUI] `elem` msgs `shouldBe` True
          lookup otherPlayer (reparked msgs) `shouldBe` Just theirs

    it "folds the answering seat's remaining options into the same map" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      let
        mine = [Label "Quick Thinking" [ClearUI], Label "Nimble" [ClearUI]]
        theirs = afterSkillTestSeat [Label "Unrelenting (1)" [ClearUI]]
      overTest $ \g ->
        g
          { gameQuestion = mapFromList [(pid, afterSkillTestSeat mine), (otherPlayer, theirs)]
          , gameRetainedQuestion = True
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled msgs -> do
          -- one AskMap holding both seats, not an Ask parked ahead of an AskMap:
          -- the table must stay free to resolve these in any order
          any (\case Ask {} -> True; _ -> False) msgs `shouldBe` False
          let question' = reparked msgs
          lookup otherPlayer question' `shouldBe` Just theirs
          lookup pid question' `shouldBe` Just (afterSkillTestSeat [Label "Nimble" [ClearUI]])

    it "still drops the other seats when the question is not retained" . gameTest $ \self -> do
      pid <- getPlayer (toId self)
      overTest $ \g ->
        g
          { gameQuestion =
              mapFromList
                [ (pid, ChooseOne [Label "mine" [ClearUI]])
                , (otherPlayer, ChooseOne [Label "theirs" [ClearUI]])
                ]
          , gameRetainedQuestion = False
          }

      game <- getGame
      liftIO (handleAnswerPure game pid (answerFirstChoice pid)) >>= \case
        Unhandled reason -> expectationFailure $ "answer rejected: " <> show reason
        Handled msgs -> do
          reparked msgs `shouldBe` mempty
          [m | AskMap m <- msgs] `shouldBe` []
