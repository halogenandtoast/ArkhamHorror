module Arkham.Enemy.Cards.StowawayDrone (stowawayDrone) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StowawayDrone = StowawayDrone EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stowawayDrone :: EnemyCard StowawayDrone
stowawayDrone = enemy StowawayDrone Cards.stowawayDrone

instance HasAbilities StowawayDrone where
  getAbilities (StowawayDrone a) =
    extend1 a
      $ restricted a 1 (exists $ NonEliteEnemy <> not_ (be a) <> EnemyAt (locationWithEnemy a))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage StowawayDrone where
  runMessage msg e@(StowawayDrone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Place 1 doom on another non-Elite enemy at this location. Then, discard
      -- Stowaway Drone." The discard happens either way.
      others <- select $ NonEliteEnemy <> not_ (be attrs) <> EnemyAt (locationWithEnemy attrs)
      chooseOrRunOneM iid $ targets others \eid -> do
        placeDoom (attrs.ability 1) eid 1
        toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> StowawayDrone <$> liftRunMessage msg attrs
