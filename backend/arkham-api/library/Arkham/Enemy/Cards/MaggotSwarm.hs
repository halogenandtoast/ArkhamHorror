module Arkham.Enemy.Cards.MaggotSwarm (maggotSwarm) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhenM)
import Arkham.Keyword
import Arkham.Matcher

newtype MaggotSwarm = MaggotSwarm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

maggotSwarm :: EnemyCard MaggotSwarm
maggotSwarm = enemy MaggotSwarm Cards.maggotSwarm (3, Static 2, 3) (0, 1)

instance HasModifiersFor MaggotSwarm where
  getModifiersFor (MaggotSwarm a) = do
    modifySelect a (locationWithEnemy a) [ShroudModifier 2]
    modifySelfWhenM a (a.id <=~> EnemyAt LocationWithoutClues) [AddKeyword Hunter]

instance RunMessage MaggotSwarm where
  runMessage msg (MaggotSwarm attrs) = MaggotSwarm <$> runMessage msg attrs
