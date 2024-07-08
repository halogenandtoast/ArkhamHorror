module Arkham.Location.Cards.SecurityOffice_128 (securityOffice_128, SecurityOffice_128 (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (securityOffice_128)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype SecurityOffice_128 = SecurityOffice_128 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice_128 :: LocationCard SecurityOffice_128
securityOffice_128 = location SecurityOffice_128 Cards.securityOffice_128 2 (PerPlayer 1)

instance HasAbilities SecurityOffice_128 where
  getAbilities (SecurityOffice_128 x) =
    extendRevealed
      x
      [playerLimit PerTurn $ restrictedAbility x 1 Here (ActionAbility [] $ ActionCost 2)]

instance RunMessage SecurityOffice_128 where
  runMessage msg l@(SecurityOffice_128 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] (basic #any) (DrawFound iid 1)
      pure l
    _ -> SecurityOffice_128 <$> liftRunMessage msg attrs
