module Arkham.Location.Cards.Balcony (balcony) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Balcony = Balcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balcony :: LocationCard Balcony
balcony = location Balcony Cards.balcony 2 (PerPlayer 1)

instance HasAbilities Balcony where
  getAbilities (Balcony a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ MoveAction #after You (be a) (locationIs Cards.theatre)

instance RunMessage Balcony where
  runMessage msg l@(Balcony attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      pure l
    _ -> Balcony <$> liftRunMessage msg attrs
