module Arkham.Scenario.Scenarios.DealingsInTheDarkSpec (spec) where

import Arkham.Campaigns.TheScarletKeys.Key (TheScarletKeysKey (Time))
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Story qualified as Story
import Arkham.Projection
import Arkham.Story.Cards qualified as Stories
import Arkham.Story.Types (Field (..))
import TestImport.New

{- | Dealings in the Dark seeds the cult's clue pool on The Unveiling during
setup, based on how much /time/ has passed in the campaign log:

* 10 or fewer: no clues
* 11–17: half the number of investigators, rounded up
* 18–24: the number of investigators
* 25 or more: twice the number of investigators

See issue #5255, where none of these clues were placed at all.
-}
spec :: Spec
spec = describe "Dealings in the Dark" $ do
  context "setup places clues on The Unveiling based on time passed" $ do
    for_ [(5, 0), (13, 1), (20, 2), (30, 4)] \(time, expected) ->
      it ("places " <> show expected <> " clues when time is " <> show time)
        . scenarioTest "09566"
        $ \_ -> do
          -- two investigators: the default plus one
          void $ addInvestigator Investigators.rolandBanks
          recordCount Time time
          pushAndRun Setup
          -- Setup pauses on its instructions. Isolate the act prerequisite and
          -- story messages so the test does not continue into the first turn.
          queued <- peekQueue
          let isUnveilingMessage = \case
                SetActDeck -> True
                StoryMessage (Story.PlaceStory card _) -> toCardCode card == Stories.theUnveiling.cardCode
                PlaceTokens _ (StoryTarget sid) _ _ -> sid == StoryId Stories.theUnveiling.cardCode
                _ -> False
          setQueue $ filter isUnveilingMessage queued
          overTest (questionL .~ mempty)
          runMessages
          theUnveiling <- selectJust $ Matcher.storyIs Stories.theUnveiling
          clues <- field StoryClues theUnveiling
          clues `shouldBe` expected
