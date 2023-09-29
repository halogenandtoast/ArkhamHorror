module Arkham.Enemy.Cards.SlimeCoveredDhole (
  SlimeCoveredDhole (..),
  slimeCoveredDhole,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype SlimeCoveredDhole = SlimeCoveredDhole EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slimeCoveredDhole :: EnemyCard SlimeCoveredDhole
slimeCoveredDhole =
  enemyWith
    SlimeCoveredDhole
    Cards.slimeCoveredDhole
    (2, Static 3, 3)
    (1, 1)
    ( (preyL .~ Prey LowestRemainingHealth)
        . (spawnAtL ?~ SpawnLocation (LocationWithoutTrait Bayou))
    )

instance HasAbilities SlimeCoveredDhole where
  getAbilities (SlimeCoveredDhole attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyEnters Timing.When (LocationWithInvestigator Anyone)
          $ EnemyWithId
          $ toId attrs
      ]

instance RunMessage SlimeCoveredDhole where
  runMessage msg e@(SlimeCoveredDhole attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorsAtSameLocation attrs
      e
        <$ pushAll
          [ InvestigatorAssignDamage iid source DamageAny 0 1
          | iid <- investigatorIds
          ]
    _ -> SlimeCoveredDhole <$> runMessage msg attrs
