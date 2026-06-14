module Arkham.Location.Cards.ObsidianCliffs (obsidianCliffs) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ObsidianCliffs = ObsidianCliffs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianCliffs :: LocationCard ObsidianCliffs
obsidianCliffs = location ObsidianCliffs Cards.obsidianCliffs 4 (Static 1)

instance HasAbilities ObsidianCliffs where
  getAbilities (ObsidianCliffs a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage ObsidianCliffs where
  runMessage msg l@(ObsidianCliffs attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- TODO: "Choose an adjacent open sky." The Summit deck and "open sky"
      -- placeholder cards have no engine support yet, so we cannot present the
      -- adjacent-open-sky targeting here. Once that infrastructure exists, ask
      -- the investigator to choose an adjacent open sky before the test and
      -- thread the choice through to the placement below.
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      -- Best effort: put the set-aside Glyph Orrery (rune_d) into play. The real
      -- effect places it into the chosen adjacent open sky and slides that open
      -- sky onto the top of the Summit deck.
      -- TODO: place the Glyph Orrery into the chosen open sky and place that
      -- open sky on top of the Summit deck once Summit-deck/open-sky/sliding
      -- support exists.
      placeSetAsideLocation_ Cards.glyphOrrery
      pure l
    _ -> ObsidianCliffs <$> liftRunMessage msg attrs
