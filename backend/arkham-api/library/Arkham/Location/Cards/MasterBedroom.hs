module Arkham.Location.Cards.MasterBedroom (masterBedroom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (SilverTwilight))

newtype MasterBedroom = MasterBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroom :: LocationCard MasterBedroom
masterBedroom = location MasterBedroom Cards.masterBedroom 3 (PerPlayer 1)

instance HasAbilities MasterBedroom where
  getAbilities (MasterBedroom a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( exists (investigatorAt $ toId a)
            <> exists (EnemyWithTrait SilverTwilight <> EnemyWithoutModifier CannotPlaceDoomOnThis)
        )
      $ forced
      $ RoundEnds #when

instance RunMessage MasterBedroom where
  runMessage msg l@(MasterBedroom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemies <-
        select
          $ NearestEnemyToLocationFallback
            attrs.id
            (EnemyWithTrait SilverTwilight <> EnemyWithoutModifier CannotPlaceDoomOnThis)
      leadChooseOrRunOneM $ targets enemies $ placeDoomOn (attrs.ability 1) 1
      pure l
    _ -> MasterBedroom <$> liftRunMessage msg attrs
