module Arkham.Asset.Assets.DreamersChronicleSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.Token
import TestImport.New

chooseChronicle :: TestAppT ()
chooseChronicle = chooseOptionMatching "choose Dreamer's Chronicle's option" \case
  Label lbl _ -> "Dreamer's Chronicle" `isInfixOf` lbl
  _ -> False

takeHorror :: TestAppT ()
takeHorror = chooseOptionMatching "take 1 horror for an additional clue" \case
  Label lbl _ -> "takeHorror" `isInfixOf` lbl
  _ -> False

spec :: Spec
spec = describe "Dreamer's Chronicle" do
  it "takes 1 horror to discover 1 additional clue" . gameTest $ \self -> do
    withProp @"intellect" 5 self
    chronicle <- self `putAssetIntoPlay` Assets.dreamersChronicle
    location <- testLocation & prop @"clues" 2 & prop @"shroud" 1
    self `moveTo` location
    setChaosTokens [Zero]
    [doInvestigate] <- self `getActionsFrom` chronicle
    self `useAbility` doInvestigate
    startSkillTest
    applyResults
    chooseChronicle
    takeHorror
    applyAllHorror
    self.horror `shouldReturn` 1
    location.clues `shouldReturn` 0
    self.clues `shouldReturn` 2

  -- Issue #5247: the additional clue must be discovered as part of the *same*
  -- discovery as the investigation's clue, so Science Hall's "After you discover
  -- 1 or more clues here, discard 1 card" triggers once, not twice.
  it "discovers the additional clue as part of the same discovery" . gameTest $ \self -> do
    withProp @"intellect" 5 self
    flashlight <- genCard Assets.flashlight
    knife <- genCard Assets.knife
    withProp @"hand" [flashlight, knife] self
    chronicle <- self `putAssetIntoPlay` Assets.dreamersChronicle
    (scienceHall, placement) <- placeLocationCard Locations.scienceHall
    run placement
    self `moveTo` scienceHall
    run $ PlaceTokens (TestSource mempty) (toTarget scienceHall) Clue 1
    field LocationClues scienceHall `shouldReturn` 2
    setChaosTokens [Zero]
    [doInvestigate] <- self `getActionsFrom` chronicle
    self `useAbility` doInvestigate
    startSkillTest
    applyResults
    chooseChronicle
    takeHorror
    applyAllHorror
    useForcedAbility
    chooseTarget flashlight
    self.clues `shouldReturn` 2
    field LocationClues scienceHall `shouldReturn` 0
    self.hand `shouldReturn` [knife]
    -- a second discovery window would leave Science Hall's forced ability
    -- waiting to be triggered again
    assertHasNoReaction
