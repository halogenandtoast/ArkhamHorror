module Arkham.Scenario.Scenarios.DeadHeatSpec (spec) where

import Arkham.Campaigns.TheScarletKeys.Key (TheScarletKeysKey (Time))
import Arkham.Classes.HasGame (getGame)
import Arkham.Helpers.Log (scenarioCount)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.ScenarioLogKey (ScenarioCountKey (CiviliansSlain))
import TestImport.New

{- | Dead Heat's setup slays civilians based on how much /time/ has passed in the
campaign log, after placing one more civilian than there are investigators on
every location:

* 10 or fewer: none are slain
* 11–17: 1 {perPlayer} are slain
* 18–24: 2 {perPlayer} are slain

See issue #5433, where the setup removed the tokens but never recorded them as
slain, so the scenario reference stayed empty, {skull} stayed at zero, and the
"Take That, Ghulat" achievement (no civilian slain) was wrongly earned.
-}
spec :: Spec
spec = describe "Dead Heat" $ do
  context "setup records slain civilians based on time passed" $ do
    for_ [(5, 0), (13, 2), (20, 4)] \(time, expected) ->
      it ("slays " <> show expected <> " civilians when time is " <> show time)
        . scenarioTest "09520"
        $ \_ -> do
          -- two investigators: the default plus one
          void $ addInvestigator Investigators.rolandBanks
          recordCount Time time
          pushAndRun Setup
          -- Setup pauses on its instructions. Keep only the locations and the
          -- civilian step so the test does not continue into the first turn.
          queued <- peekQueue
          let isCivilianSetupMessage = \case
                PlaceLocation {} -> True
                DoStep 2 Setup -> True
                _ -> False
          setQueue $ filter isCivilianSetupMessage queued
          overTest (questionL .~ mempty)
          runMessages
          answerEveryChooseN
          scenarioCount CiviliansSlain `shouldReturn` expected

{- | Answer any pending @ChooseN@ by repeatedly taking its first option, the way
'Entity.Answer' unwinds one, until no @ChooseN@ is left.
-}
answerEveryChooseN :: TestAppT ()
answerEveryChooseN = do
  questionMap <- gameQuestion <$> getGame
  case mapToList questionMap of
    [(pid, question)] | ChooseN n (msg : rest) <- stripQuestionWrappers question -> do
      overTest (questionL .~ mempty)
      queued <- peekQueue
      setQueue
        $ (uiToRun msg : [Ask pid (ChooseN (n - 1) rest) | n > 1, notNull rest])
          <> queued
      runMessages
      answerEveryChooseN
    _ -> pure ()
