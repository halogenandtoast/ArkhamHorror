module Arkham.Location.Cards.GareDOrsay (gareDOrsay) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait

newtype GareDOrsay = GareDOrsay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gareDOrsay :: LocationCard GareDOrsay
gareDOrsay = location GareDOrsay Cards.gareDOrsay 4 (PerPlayer 1)

instance HasAbilities GareDOrsay where
  getAbilities (GareDOrsay a) = extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage GareDOrsay where
  runMessage msg l@(GareDOrsay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      rails <- select $ LocationWithTrait Rail <> not_ (be attrs)
      chooseTargetM iid rails (moveTo (attrs.ability 1) iid)
      pure l
    _ -> GareDOrsay <$> liftRunMessage msg attrs
