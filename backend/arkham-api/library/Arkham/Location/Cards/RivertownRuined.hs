module Arkham.Location.Cards.RivertownRuined (rivertownRuined) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Helpers (decreaseFloodLevel)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RivertownRuined = RivertownRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertownRuined :: LocationCard RivertownRuined
rivertownRuined = location RivertownRuined Cards.rivertownRuined 2 (Static 1)

drainable :: LocationAttrs -> LocationMatcher
drainable a = oneOf [be a, connectedTo (be a)] <> FloodedLocation

instance HasAbilities RivertownRuined where
  getAbilities (RivertownRuined a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ skillTestAbility
      $ restricted a 1 (Here <> exists (drainable a)) actionAbility

instance RunMessage RivertownRuined where
  runMessage msg l@(RivertownRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) attrs [#agility, #intellect] (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      candidates <- select $ drainable attrs
      chooseTargetM iid candidates decreaseFloodLevel
      pure l
    _ -> RivertownRuined <$> liftRunMessage msg attrs
