module Arkham.Location.Cards.Stairwell (stairwell, Stairwell (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Trait (Trait (Basement))

newtype Stairwell = Stairwell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stairwell :: LocationCard Stairwell
stairwell = location Stairwell Cards.stairwell 3 (PerPlayer 1)

instance HasAbilities Stairwell where
  getAbilities (Stairwell attrs) =
    extendRevealed
      attrs
      [ skillTestAbility
          $ mkAbility attrs 1
          $ freeReaction
          $ moves #after You (not_ $ LocationWithTrait Basement) attrs
      , groupLimit PerTestOrAbility $ mkAbility attrs 2 $ forced $ BecomesInfested #after (be attrs)
      ]

instance RunMessage Stairwell where
  runMessage msg l@(Stairwell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      basementLocations <- select $ LocationWithTrait Basement
      sid <- getRandom
      chooseTargetM iid basementLocations \basementLocation -> do
        moveTo (attrs.ability 1) iid basementLocation
        beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 2
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      makeInfestationTest
      pure l
    _ -> Stairwell <$> liftRunMessage msg attrs
