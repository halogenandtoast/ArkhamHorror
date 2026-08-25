module Arkham.Homebrew.CircusExMortis.Locations.CraneCar (craneCar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype CraneCar = CraneCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

craneCar :: LocationCard CraneCar
craneCar = location CraneCar Cards.craneCar 3 (Static 2)

instance HasAbilities CraneCar where
  getAbilities (CraneCar a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (thisExists a LocationWithoutClues) actionAbility

instance RunMessage CraneCar where
  runMessage msg l@(CraneCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select Anyone
      enemies <- select NonEliteEnemy
      chooseOrRunOneM iid do
        targets investigators \iid' -> do
          selectEach (enemyEngagedWith iid') (disengageEnemy iid')
          moveTo (attrs.ability 1) iid' attrs
        targets enemies \eid -> do
          disengageFromAll eid
          enemyMoveTo (attrs.ability 1) eid attrs
      pure l
    _ -> CraneCar <$> liftRunMessage msg attrs
