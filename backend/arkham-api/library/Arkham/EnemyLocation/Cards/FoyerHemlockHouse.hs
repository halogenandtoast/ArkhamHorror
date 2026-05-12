module Arkham.EnemyLocation.Cards.FoyerHemlockHouse where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Location.Helpers qualified as Helpers
import Arkham.Matcher

newtype FoyerHemlockHouse = FoyerHemlockHouse EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerHemlockHouse :: EnemyLocationCard FoyerHemlockHouse
foyerHemlockHouse =
  enemyLocationWith FoyerHemlockHouse Cards.foyerHemlockHouse (2, Static 3, 3) (1, 1)
    $ baseL
    %~ \la -> la {locationShroud = Just (Static 2)}

instance HasAbilities FoyerHemlockHouse where
  getAbilities (FoyerHemlockHouse a) =
    getAbilities a
      <> [ -- "Forced - When this enemy-location is revealed: It attacks each
           -- investigator at this location."
           mkAbility a 1
             $ forced
             $ FlipLocation #after Anyone (LocationWithId a.id)
         , -- "[action]: Resign. You flee the house."
           Helpers.resignAction a
         ]

instance RunMessage FoyerHemlockHouse where
  runMessage msg el@(FoyerHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    _ -> FoyerHemlockHouse <$> liftRunMessage msg attrs
