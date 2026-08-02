module Arkham.Location.Cards.ObsidianCliffs (obsidianCliffs) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (getSkillTestTargetedLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Window (getBatchId)

newtype ObsidianCliffs = ObsidianCliffs LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianCliffs :: LocationCard ObsidianCliffs
obsidianCliffs = location ObsidianCliffs Cards.obsidianCliffs 4 (Static 1)

instance HasAbilities ObsidianCliffs where
  getAbilities (ObsidianCliffs a) =
    if a.revealed
      then
        extendRevealed1 a
          $ skillTestAbility
          $ restricted
            a
            1
            ( Here
                <> exists (isOpenSky <> LocationWithDistanceFromAtMost 1 (be a) Anywhere)
                <> SetAsideCardExists (cardIs Cards.glyphOrrery)
            )
            actionAbility
      else extendUnrevealed1 a (summitEntry a 9)

instance RunMessage ObsidianCliffs where
  runMessage msg l@(ObsidianCliffs attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 9 (getBatchId -> batchId) _ -> do
      summitEntryToll attrs 9 iid batchId
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 9 -> True) -> do
      summitEntryFailed attrs 9 iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Choose an adjacent open sky" happens before the test; remember it so the
      -- Orrery lands where the investigator picked.
      openSkies <- getAdjacentOpenSky attrs.id
      chooseTargetM iid openSkies \sky -> do
        -- The chosen sky rides along as the test's target so the Orrery lands
        -- where the investigator picked, not somewhere re-derived on success.
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) sky #intellect (Fixed 3)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      -- "Put the set-aside Glyph Orrery into play in the chosen open sky (placing
      -- that open sky on top of the Summit deck)."
      whenJustM getSkillTestTargetedLocation \sky -> do
        orrery <- getSetAsideCard Cards.glyphOrrery
        placeInOpenSky orrery sky
      pure l
    _ -> ObsidianCliffs <$> liftRunMessage msg attrs
