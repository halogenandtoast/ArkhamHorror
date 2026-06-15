module Arkham.EnemyLocation.Cards.LivingFoyerHemlockHouse where

import Arkham.Ability
import Arkham.Constants
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Location.Helpers qualified as Helpers
import Arkham.Matcher

newtype LivingFoyerHemlockHouse = LivingFoyerHemlockHouse EnemyLocationAttrs
  deriving anyclass (IsEnemyLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingFoyerHemlockHouse :: EnemyLocationCard LivingFoyerHemlockHouse
livingFoyerHemlockHouse =
  enemyLocationWith LivingFoyerHemlockHouse Cards.livingFoyerHemlockHouse (2, PerPlayer 3, 3) (2, 0)
    $ baseL
    %~ \la -> la {locationShroud = Just (Static 2)}

instance HasAbilities LivingFoyerHemlockHouse where
  getAbilities (LivingFoyerHemlockHouse a) =
    extend
      a
      [ mkAbility a 1 $ forced $ FlipLocation #after Anyone (LocationWithId a.id)
      , Helpers.resignAction a
      ]

instance RunMessage LivingFoyerHemlockHouse where
  runMessage msg el@(LivingFoyerHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      sendMessage attrs $ Do EnemiesAttack
      pure el
    UseThisAbility iid (isSource attrs -> True) ResignAbility -> do
      resign iid
      pure el
    _ -> LivingFoyerHemlockHouse <$> liftRunMessage msg attrs
