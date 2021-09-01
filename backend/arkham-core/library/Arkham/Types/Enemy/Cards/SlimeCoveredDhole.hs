module Arkham.Types.Enemy.Cards.SlimeCoveredDhole
  ( SlimeCoveredDhole(..)
  , slimeCoveredDhole
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype SlimeCoveredDhole = SlimeCoveredDhole EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slimeCoveredDhole :: EnemyCard SlimeCoveredDhole
slimeCoveredDhole = enemyWith
  SlimeCoveredDhole
  Cards.slimeCoveredDhole
  (2, Static 3, 3)
  (1, 1)
  ((preyL .~ LowestRemainingHealth) . (spawnAtL ?~ LocationWithoutTrait Bayou))

instance HasAbilities env SlimeCoveredDhole where
  getAbilities iid window (SlimeCoveredDhole attrs) =
    withBaseAbilities iid window attrs $ pure
      [ mkAbility attrs 1
        $ ForcedAbility
        $ EnemyEnters Timing.When (LocationWithInvestigator Anyone)
        $ EnemyWithId
        $ toId attrs
      ]

instance EnemyRunner env => RunMessage env SlimeCoveredDhole where
  runMessage msg e@(SlimeCoveredDhole attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      investigatorIds <-
        selectList $ InvestigatorAt $ LocationWithId $ enemyLocation attrs
      e <$ pushAll
        [ InvestigatorAssignDamage iid source DamageAny 0 1
        | iid <- investigatorIds
        ]
    _ -> SlimeCoveredDhole <$> runMessage msg attrs
