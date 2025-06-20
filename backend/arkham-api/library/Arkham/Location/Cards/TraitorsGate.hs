module Arkham.Location.Cards.TraitorsGate (traitorsGate, TraitorsGate(..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.RiddlesAndRain.Helpers (scenarioI18n)

newtype TraitorsGate = TraitorsGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

traitorsGate :: LocationCard TraitorsGate
traitorsGate = location TraitorsGate Cards.traitorsGate 4 (PerPlayer 1)

instance HasAbilities TraitorsGate where
  getAbilities (TraitorsGate a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage TraitorsGate where
  runMessage msg l@(TraitorsGate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ do
        for_ [#combat, #agility] \skill ->
          skillLabeled skill $ beginSkillTest sid iid (attrs.ability 1) iid skill (Fixed 5)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      -- TODO: reveal the Tower of London
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> TraitorsGate <$> liftRunMessage msg attrs
