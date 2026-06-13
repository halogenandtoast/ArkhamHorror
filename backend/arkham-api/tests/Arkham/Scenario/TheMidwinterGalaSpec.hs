module Arkham.Scenario.TheMidwinterGalaSpec (spec) where

import Arkham.CampaignLogKey
import Arkham.Classes.HasGame (getGame)
import Arkham.Question
import Data.Text qualified as T
import TestImport

-- | When the Midwinter Gala is played as a side-story and the investigators
-- survived (keeping the Jewel of Sarnath), the setup of each subsequent
-- scenario should offer to shuffle the Jewel of Sarnath into the encounter
-- deck. See issue #4790.
spec :: Spec
spec = describe "The Midwinter Gala (Jewel of Sarnath carry-over)" do
  it "offers to shuffle the Jewel of Sarnath into the encounter deck during the setup of a subsequent scenario" . scenarioTest "02041" $ \_ -> do
    record TheInvestigatorsSurvivedTheMidwinterGala
    pushAndRun EndSetup
    labels <- pendingLabels
    liftIO $ do
      any ("shuffleIn" `T.isInfixOf`) labels `shouldBe` True
      any ("dontShuffle" `T.isInfixOf`) labels `shouldBe` True

-- | The labels of every option in every currently-pending question.
pendingLabels :: TestAppT [Text]
pendingLabels = do
  questions <- toList . gameQuestion <$> getGame
  pure $ concatMap questionLabelTexts questions

questionLabelTexts :: Question Message -> [Text]
questionLabelTexts = \case
  QuestionLabel _ _ q -> questionLabelTexts q
  ChooseOne uis -> mapMaybe uiLabel uis
  _ -> []
 where
  uiLabel = \case
    Label l _ -> Just l
    _ -> Nothing
