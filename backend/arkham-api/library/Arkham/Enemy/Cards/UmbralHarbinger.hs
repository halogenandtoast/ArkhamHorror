module Arkham.Enemy.Cards.UmbralHarbinger (umbralHarbinger) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype UmbralHarbinger = UmbralHarbinger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umbralHarbinger :: EnemyCard UmbralHarbinger
umbralHarbinger = enemy UmbralHarbinger Cards.umbralHarbinger (3, Static 5, 1) (1, 1)

instance HasAbilities UmbralHarbinger where
  getAbilities (UmbralHarbinger a) =
    extend1 a
      $ restricted a 1 (exists $ InPlayEnemy #cultist)
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) AnySource

instance RunMessage UmbralHarbinger where
  runMessage msg e@(UmbralHarbinger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        cultists <- select $ NearestEnemyToLocationFallback lid #cultist
        chooseTargetM iid cultists $ placeDoomOn attrs 1
      pure e
    _ -> UmbralHarbinger <$> liftRunMessage msg attrs
