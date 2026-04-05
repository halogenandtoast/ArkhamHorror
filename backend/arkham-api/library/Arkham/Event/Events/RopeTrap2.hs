module Arkham.Event.Events.RopeTrap2 (ropeTrap2) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Matcher

newtype RopeTrap2 = RopeTrap2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ropeTrap2 :: EventCard RopeTrap2
ropeTrap2 = event RopeTrap2 Cards.ropeTrap2

instance HasAbilities RopeTrap2 where
  getAbilities (RopeTrap2 a) = case a.attachedTo.location of
    Just lid ->
      [controlled_ a 1 $ triggered (EnemyEnters #after (LocationWithId lid) AnyEnemy) (exhaust a)]
    _ -> []

instance RunMessage RopeTrap2 where
  runMessage msg e@(RopeTrap2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (enteringEnemy -> eid) _ -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      requestChaosTokens iid (attrs.ability 1) 1
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      when (any (\t -> t.face `elem` [Skull, AutoFail]) tokens) do
        toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> RopeTrap2 <$> liftRunMessage msg attrs
