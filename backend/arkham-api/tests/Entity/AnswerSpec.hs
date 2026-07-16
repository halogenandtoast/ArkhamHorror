module Entity.AnswerSpec (spec) where

import Arkham.Campaign (lookupCampaign)
import Arkham.Campaign.Types (campaignStep)
import Arkham.CampaignStep qualified as CS
import Arkham.Difficulty
import Entity.Answer
import TestImport.New

spec :: Spec
spec = describe "CampaignStepAnswer" do
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
          $ "stale campaign answer was accepted: " <> show messages
