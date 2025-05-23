module Arkham.Location.Cards.NorthsideTrainStation (northsideTrainStation) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (northsideTrainStation)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait

newtype NorthsideTrainStation = NorthsideTrainStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northsideTrainStation :: LocationCard NorthsideTrainStation
northsideTrainStation = location NorthsideTrainStation Cards.northsideTrainStation 2 (PerPlayer 1)

instance HasAbilities NorthsideTrainStation where
  getAbilities (NorthsideTrainStation a) =
    extendRevealed1 a $ playerLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage NorthsideTrainStation where
  runMessage msg l@(NorthsideTrainStation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithTrait Arkham
      chooseTargetM iid locations $ moveTo (attrs.ability 1) iid
      pure l
    _ -> NorthsideTrainStation <$> liftRunMessage msg attrs
