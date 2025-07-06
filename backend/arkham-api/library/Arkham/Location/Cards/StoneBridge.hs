module Arkham.Location.Cards.StoneBridge (stoneBridge) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype StoneBridge = StoneBridge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneBridge :: LocationCard StoneBridge
stoneBridge = locationWith StoneBridge Cards.stoneBridge 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities StoneBridge where
  getAbilities (StoneBridge a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here actionAbility
      , restricted
          a
          2
          (Here <> exists (CanMoveToLocation You (a.ability 2) ("Stone Bridge" <> not_ (be a))))
          actionAbility
      ]

instance RunMessage StoneBridge where
  runMessage msg l@(StoneBridge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) iid kind (Fixed 4)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ls <- select $ CanMoveToLocation You (attrs.ability 2) ("Stone Bridge" <> not_ (be attrs))
      chooseTargetM iid ls $ moveTo (attrs.ability 2) iid
      pure l
    _ -> StoneBridge <$> liftRunMessage msg attrs
