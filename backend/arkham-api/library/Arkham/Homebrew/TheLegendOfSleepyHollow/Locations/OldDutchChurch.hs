module Arkham.Homebrew.TheLegendOfSleepyHollow.Locations.OldDutchChurch (oldDutchChurch) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log (remember)

newtype OldDutchChurch = OldDutchChurch LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldDutchChurch :: LocationCard OldDutchChurch
oldDutchChurch = location OldDutchChurch Cards.oldDutchChurch 3 (PerPlayer 2)

-- | "The Headless Horseman cannot move to Old Dutch Church."
instance HasModifiersFor OldDutchChurch where
  getModifiersFor (OldDutchChurch a) =
    modifySelect a headlessHorseman [CannotEnter a.id]

instance HasAbilities OldDutchChurch where
  getAbilities (OldDutchChurch a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "oldDutchChurch.name"
      $ skillTestAbility
      $ restricted a 1 Here actionAbility

instance RunMessage OldDutchChurch where
  runMessage msg l@(OldDutchChurch attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed $ 2 + attrs.clues)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember learnedTheHorsemansName
      pure l
    _ -> OldDutchChurch <$> liftRunMessage msg attrs
