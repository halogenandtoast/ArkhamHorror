module Arkham.Enemy.Cards.GiantAlbinoPenguin (giantAlbinoPenguin) where

import Arkham.Cost
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype GiantAlbinoPenguin = GiantAlbinoPenguin EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

giantAlbinoPenguin :: EnemyCard GiantAlbinoPenguin
giantAlbinoPenguin = enemy GiantAlbinoPenguin Cards.giantAlbinoPenguin (2, Static 3, 2) (1, 1)

instance HasModifiersFor GiantAlbinoPenguin where
  getModifiersFor (GiantAlbinoPenguin a) = do
    modifySelect
      a
      (locationWithEnemy a)
      [ AdditionalCostToEnter $ ActionCost 1
      , AdditionalCostToLeave $ ActionCost 1
      ]

instance RunMessage GiantAlbinoPenguin where
  runMessage msg (GiantAlbinoPenguin attrs) = GiantAlbinoPenguin <$> runMessage msg attrs
