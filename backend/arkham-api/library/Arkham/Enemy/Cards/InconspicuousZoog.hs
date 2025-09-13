module Arkham.Enemy.Cards.InconspicuousZoog (inconspicuousZoog) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.ForMovement
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement

newtype InconspicuousZoog = InconspicuousZoog EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconspicuousZoog :: EnemyCard InconspicuousZoog
inconspicuousZoog =
  enemyWith InconspicuousZoog Cards.inconspicuousZoog (2, Static 1, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt (ConnectedLocation NotForMovement)

instance HasAbilities InconspicuousZoog where
  getAbilities (InconspicuousZoog x) =
    extend1 x $ restricted x 1 isSwarmRestriction $ forced $ EnemyDefeated #when You ByAny (be x)
   where
    isSwarmRestriction = case x.placement of
      AsSwarm _ _ -> NoRestriction
      _ -> Never

instance RunMessage InconspicuousZoog where
  runMessage msg e@(InconspicuousZoog attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      matchingDon't \case
        ExcessDamage eid' _ | toId attrs == eid' -> True
        _ -> False
      case attrs.placement of
        AsSwarm host _ -> do
          connectingLocations <-
            select $ connectedFrom (locationWithEnemy host) <> LocationCanBeEnteredBy host
          unless (null connectingLocations) do
            exhaustThis host
            chooseOrRunOneM iid $ targets connectingLocations $ enemyMoveTo (attrs.ability 1) host
        _ -> error "should not trigger"
      pure e
    _ -> InconspicuousZoog <$> liftRunMessage msg attrs
