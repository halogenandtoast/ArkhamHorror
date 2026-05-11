module Arkham.EnemyLocation.Cards.LivingParlorHemlockHouse (livingParlorHemlockHouse) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Matcher

newtype LivingParlorHemlockHouse = LivingParlorHemlockHouse EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Living Parlor: Massive. Retaliate. Cannot make attacks of opportunity.
-- Living Parlor gets +2 [per_investigator] health.
livingParlorHemlockHouse :: EnemyLocationCard LivingParlorHemlockHouse
livingParlorHemlockHouse =
  enemyLocationWith
    LivingParlorHemlockHouse
    Cards.livingParlorHemlockHouse
    (2, StaticWithPerPlayer 3 2, 3)
    (1, 1)
    (\la -> la {enemyLocationBase = (enemyLocationBase la) {locationShroud = Just (Static 4)}})

instance HasAbilities LivingParlorHemlockHouse where
  getAbilities (LivingParlorHemlockHouse a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: It attacks each
           -- investigator at this location."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         ]

instance RunMessage LivingParlorHemlockHouse where
  runMessage msg el@(LivingParlorHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    _ -> LivingParlorHemlockHouse <$> liftRunMessage msg attrs
