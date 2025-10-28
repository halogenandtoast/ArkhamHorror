module Arkham.Location.Cards.TraitorsGate (traitorsGate) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TraitorsGate = TraitorsGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

traitorsGate :: LocationCard TraitorsGate
traitorsGate = symbolLabel $ location TraitorsGate Cards.traitorsGate 4 (PerPlayer 1)

instance HasAbilities TraitorsGate where
  getAbilities (TraitorsGate a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage TraitorsGate where
  runMessage msg l@(TraitorsGate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#combat, #agility] (Fixed 5)
      pure l
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      selectOne (locationIs Cards.towerOfLondon) >>= traverse_ reveal
      pure l
    _ -> TraitorsGate <$> liftRunMessage msg attrs
