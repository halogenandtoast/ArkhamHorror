module Arkham.Scenario.ShadesOfSufferingSpec (spec) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Classes.HasChaosTokenValue
import Arkham.Difficulty
import Arkham.Helpers.Calculation (calculate)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Placement
import Arkham.Projection (fieldMap)
import Arkham.Token (countTokens)
import TestImport.New

{- | Shades of Suffering's {skull} is -X, where X is half the charges on The Shade
Reaper (rounded up), and its {cultist} places a charge on the key. The key leaves
play with its bearer -- Tzu San Niang defeated, or an eliminated investigator --
and the scenario reference is still consulted afterwards: the skill-test window
prices every face, and the test that defeated her is still being resolved. With
the key out of play there are no charges to count or place.
-}
spec :: Spec
spec = describe "Shades of Suffering" $ do
  context "{skull}" $ do
    it "is -X where X is half the charges on The Shade Reaper, rounded up" . scenarioTest "09660" $ \self -> do
      theShadeReaper <- putTheShadeReaperIntoPlay self
      run $ PlaceTokens (toSource self) (toTarget theShadeReaper) #charge 3
      skullValue self `shouldReturn` -2

    it "is 0 while The Shade Reaper is not in play" . scenarioTest "09660" $ \self -> do
      skullValue self `shouldReturn` 0

  context "{cultist} on Hard/Expert" $ do
    it "places a charge on The Shade Reaper"
      . scenarioTestWithDifficulty Investigators.jennyBarnes Hard "09660"
      $ \self -> do
        theShadeReaper <- putTheShadeReaperIntoPlay self
        revealCultist self
        fieldMap ScarletKeyTokens (countTokens #charge) theShadeReaper `shouldReturn` 1

    it "does nothing while The Shade Reaper is not in play"
      . scenarioTestWithDifficulty Investigators.jennyBarnes Hard "09660"
      $ \self -> do
        revealCultist self
        selectAny ScarletKeyAny `shouldReturn` False

-- | The key enters play on the investigator, as it does once taken from Tzu San Niang.
putTheShadeReaperIntoPlay :: Investigator -> TestAppT ScarletKeyId
putTheShadeReaperIntoPlay self = do
  card <- genCard Keys.theShadeReaper
  run $ CreateScarletKeyAt card (AttachedToInvestigator $ toId self)
  selectJust $ scarletKeyIs Keys.theShadeReaper

skullValue :: Investigator -> TestAppT Int
skullValue self =
  getChaosTokenValue (toId self) Skull () >>= \case
    ChaosTokenValue _ (CalculatedModifier c) -> calculate c
    ChaosTokenValue _ other -> error $ "unexpected {skull} modifier: " <> show other

revealCultist :: Investigator -> TestAppT ()
revealCultist self = do
  setChaosTokens [Cultist]
  sid <- getRandom
  run $ beginSkillTest sid self #willpower 1
  startSkillTest
  applyResults
