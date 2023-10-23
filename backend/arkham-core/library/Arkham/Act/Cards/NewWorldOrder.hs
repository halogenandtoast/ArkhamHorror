module Arkham.Act.Cards.NewWorldOrder (
  NewWorldOrder (..),
  newWorldOrder,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Timing qualified as Timing

newtype NewWorldOrder = NewWorldOrder ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newWorldOrder :: ActCard NewWorldOrder
newWorldOrder = act (2, A) NewWorldOrder Cards.newWorldOrder Nothing

instance HasAbilities NewWorldOrder where
  getAbilities (NewWorldOrder x) =
    withBaseAbilities
      x
      [ fastAbility x 1 Free $ if maybe False (>= 3) (actBreaches x) then NoRestriction else Never
      , mkAbility x 2
          $ Objective
          $ ForcedAbility
          $ Matcher.EnemyDefeated Timing.After Anyone ByAny
          $ enemyIs Enemies.carlSanfordDeathlessFanatic
      ]

instance RunMessage NewWorldOrder where
  runMessage msg a@(NewWorldOrder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      location <- sampleLocation
      pushAll
        [ RemoveBreaches (toTarget attrs) 3
        , PlaceClues (toAbilitySource attrs 1) (toTarget location) 1
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (InvestigatorSource iid) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push R2
      pure a
    _ -> NewWorldOrder <$> runMessage msg attrs
