module Arkham.Act.Cards.DiscoveryOfALifetime (discoveryOfALifetime) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (Day (..), getCampaignDay)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Enemy (getUniqueEnemy)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Scenarios.TheThingInTheDepths.Helpers
import Arkham.Trait (Trait (Abomination))

newtype DiscoveryOfALifetime = DiscoveryOfALifetime ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discoveryOfALifetime :: ActCard DiscoveryOfALifetime
discoveryOfALifetime = act (2, A) DiscoveryOfALifetime Cards.discoveryOfALifetime Nothing

instance HasAbilities DiscoveryOfALifetime where
  getAbilities = actAbilities \a ->
    [ fastAbility
        a
        1
        (GroupClueCost (PerPlayer 1) YourLocation)
        (exists $ You <> at_ (connectedTo (LocationWithEnemy $ enemyIs Enemies.chelydranHybrid)))
    , restricted
        a
        2
        ( exists
            $ enemyIs Enemies.chelydranHybrid
            <> EnemyAt (LocationWithEnemy $ EnemyWithTrait Abomination <> ReadyEnemy)
        )
        $ forced
        $ PhaseEnds #when #enemy
    , restricted
        a
        3
        (exists $ enemyIs Enemies.chelydranHybrid <> EnemyAt (LocationInPosition startingPos))
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage DiscoveryOfALifetime where
  runMessage msg a@(DiscoveryOfALifetime attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chelydran <- getUniqueEnemy Enemies.chelydranHybrid
      withLocationOf iid $ roundModifier (attrs.ability 1) chelydran . ForcePatrol . LocationWithId
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      chelydran <- getUniqueEnemy Enemies.chelydranHybrid
      withLocationOf chelydran \lid -> do
        abominationCount <- selectCount $ ReadyEnemy <> EnemyWithTrait Abomination <> enemyAt lid
        when (abominationCount > 0) do
          nonAttackEnemyDamage Nothing (attrs.ability 2) abominationCount chelydran
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      getCampaignDay >>= \case
        Day1 -> push R2
        _ -> push R1
      pure a
    _ -> DiscoveryOfALifetime <$> liftRunMessage msg attrs
