module Arkham.Location.Cards.CongregationalChurch_209 (congregationalChurch_209) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (drawCardUnderneathAction)
import Arkham.Location.Import.Lifted

newtype CongregationalChurch_209 = CongregationalChurch_209 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congregationalChurch_209 :: LocationCard CongregationalChurch_209
congregationalChurch_209 = location CongregationalChurch_209 Cards.congregationalChurch_209 2 (PerPlayer 1)

instance HasAbilities CongregationalChurch_209 where
  getAbilities (CongregationalChurch_209 a) =
    extendRevealed
      a
      [ drawCardUnderneathAction a
      , restrictedAbility a 1 Here
          $ actionAbilityWithCost (HandDiscardCost 1 #any)
      ]

instance RunMessage CongregationalChurch_209 where
  runMessage msg l@(CongregationalChurch_209 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure l
    _ -> CongregationalChurch_209 <$> liftRunMessage msg attrs
