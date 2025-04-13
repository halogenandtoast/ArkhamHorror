module Arkham.Location.Cards.DunwichVillage_243 (dunwichVillage_243) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dunwichVillage_243)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype DunwichVillage_243 = DunwichVillage_243 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_243 :: LocationCard DunwichVillage_243
dunwichVillage_243 = location DunwichVillage_243 Cards.dunwichVillage_243 2 (Static 3)

instance HasAbilities DunwichVillage_243 where
  getAbilities (DunwichVillage_243 x) =
    withResignAction
      x
      [ restricted
          x
          1
          (Here <> exists (InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth" <> EnemyCanMove <> notAt_ (be x.id)))
          actionAbility
      | x.revealed
      ]

instance RunMessage DunwichVillage_243 where
  runMessage msg l@(DunwichVillage_243 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      broodOfYogSothoth <- getMatchingBroodOfYogSothoth $ EnemyCanMove <> notAt_ (be attrs.id)
      chooseTargetM iid broodOfYogSothoth \eid -> do
        moveTowardsMatching (attrs.ability 1) eid (be attrs)
      pure l
    _ -> DunwichVillage_243 <$> liftRunMessage msg attrs
