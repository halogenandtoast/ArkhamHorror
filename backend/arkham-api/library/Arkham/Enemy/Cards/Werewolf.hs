module Arkham.Enemy.Cards.Werewolf (werewolf) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype Werewolf = Werewolf EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

werewolf :: EnemyCard Werewolf
werewolf =
  enemy Werewolf Cards.werewolf (3, Static 6, 3) (2, 0)
    & setPrey MostRemainingHealth

instance RunMessage Werewolf where
  runMessage msg e@(Werewolf attrs) = runQueueT $ case msg of
    HunterMove eid | eid == attrs.id -> do
      sendMessage attrs (Blanked msg)
      sendMessage attrs (Blanked msg)
      pure e
    _ -> Werewolf <$> liftRunMessage msg attrs
