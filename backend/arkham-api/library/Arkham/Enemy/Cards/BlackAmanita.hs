module Arkham.Enemy.Cards.BlackAmanita (blackAmanita) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues)
import Arkham.Matcher

newtype BlackAmanita = BlackAmanita EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackAmanita :: EnemyCard BlackAmanita
blackAmanita = enemy BlackAmanita Cards.blackAmanita (2, Static 2, 3) (0, 1)

instance HasAbilities BlackAmanita where
  getAbilities (BlackAmanita a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ DiscoverClues #after You (orConnected_ (locationWithEnemy a)) (atLeast 1)

instance RunMessage BlackAmanita where
  runMessage msg e@(BlackAmanita attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      directHorror iid (attrs.ability 1) 1
      pure e
    _ -> BlackAmanita <$> liftRunMessage msg attrs
