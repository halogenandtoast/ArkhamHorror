module Arkham.Investigator.Cards.CarolynFern2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

-- Carolyn Fern (2)'s reaction: "After you heal 1 or more horror from an investigator or Ally
-- asset: Discover 1 clue at your location. (Limit once per round.)"
--
-- "After *you* heal" is performer-scoped, not ownership-scoped: the heal only has to be
-- performed by her, so it can come from a card she does not own -- e.g. Downtown (Arkham
-- Sanatorium)'s reaction, which heals with `UseAbilitySource iid (LocationSource ...) 1`.
-- Regression coverage for #5250, where the window used SourceOwnedBy (strict card ownership)
-- and so silently dropped every scenario-card heal.
--
-- Contrast with base Carolyn Fern (05001), worded "one of your card effects", which stays on
-- SourceOwnedBy per FAQ v2.5 Q033.

spec :: Spec
spec = describe "Carolyn Fern (2)" do
  context "reaction (heal horror window)" do
    it "triggers when she heals via an ability on a card she does not own (#5250)"
      . gameTestWith Investigators.carolynFern2
      $ \self -> do
        location <- testLocation & prop @"clues" 1
        self `moveTo` location
        self `addHorror` 1
        run $ HealHorror (toTarget self) (UseAbilitySource (toId self) (TestSource mempty) 1) 1
        useReaction
        self.clues `shouldReturn` 1

    it "still triggers when she heals with a card she controls"
      . gameTestWith Investigators.carolynFern2
      $ \self -> do
        location <- testLocation & prop @"clues" 1
        self `moveTo` location
        self `addHorror` 1
        aid <- self `putAssetIntoPlay` Assets.leatherCoat
        run $ HealHorror (toTarget self) (AssetSource aid) 1
        useReaction
        self.clues `shouldReturn` 1

    it "does not trigger when another investigator performs the heal"
      . gameTestWith Investigators.carolynFern2
      $ \self -> do
        location <- testLocation & prop @"clues" 1
        self `moveTo` location
        roland <- addInvestigator Investigators.rolandBanks
        roland `moveTo` location
        roland `addHorror` 1
        run $ HealHorror (toTarget roland) (UseAbilitySource (toId roland) (TestSource mempty) 1) 1
        assertNoReaction
