module Arkham.Enemy.Cards.UnderseaParasite (underseaParasite) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype UnderseaParasite = UnderseaParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaParasite :: EnemyCard UnderseaParasite
underseaParasite = enemy UnderseaParasite Cards.underseaParasite (5, Static 1, 5) (1, 0)

-- TODO: abilities
instance RunMessage UnderseaParasite where
  runMessage msg (UnderseaParasite attrs) = runQueueT $ UnderseaParasite <$> liftRunMessage msg attrs
