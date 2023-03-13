module Arkham.Act.Cards.ACircleUnbroken
  ( ACircleUnbroken(..)
  , aCircleUnbroken
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (EnemyDefeated)
import Arkham.Timing qualified as Timing

newtype ACircleUnbroken = ACircleUnbroken ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aCircleUnbroken :: ActCard ACircleUnbroken
aCircleUnbroken = act (4, A) ACircleUnbroken Cards.aCircleUnbroken Nothing

instance HasAbilities ACircleUnbroken where
  getAbilities (ACircleUnbroken x) =
    [ mkAbility x 1
      $ Objective
      $ ForcedAbility
      $ EnemyDefeated Timing.After Anyone
      $ enemyIs Enemies.anetteMason
    , restrictedAbility
        x
        2
        (LocationExists
        $ locationIs Locations.witchesCircle
        <> LocationWithoutClues
        )
      $ Objective
      $ ForcedAbility AnyWindow
    ]

instance RunMessage ACircleUnbroken where
  runMessage msg a@(ACircleUnbroken attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      defeatedAnette <- selectNone $ enemyIs Enemies.anetteMason
      push $ scenarioResolution $ if defeatedAnette then 1 else 2
      pure a
    _ -> ACircleUnbroken <$> runMessage msg attrs
