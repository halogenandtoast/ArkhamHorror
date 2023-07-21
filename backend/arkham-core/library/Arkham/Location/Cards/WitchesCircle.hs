module Arkham.Location.Cards.WitchesCircle (
  witchesCircle,
  WitchesCircle (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Phase
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Witch))

newtype WitchesCircle = WitchesCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchesCircle :: LocationCard WitchesCircle
witchesCircle = location WitchesCircle Cards.witchesCircle 3 (PerPlayer 3)

instance HasAbilities WitchesCircle where
  getAbilities (WitchesCircle a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          ( EnemyCriteria $
              EnemyExists $
                UnengagedEnemy
                  <> ReadyEnemy
                  <> EnemyWithTrait Witch
                  <> NotEnemy (EnemyAt $ LocationWithId $ toId a)
          )
          $ ForcedAbility
          $ PhaseBegins Timing.After
          $ PhaseIs EnemyPhase
      ]

instance RunMessage WitchesCircle where
  runMessage msg l@(WitchesCircle attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      anetteMason <- getSetAsideCard Enemies.anetteMason
      pushM $ createEnemyAt_ anetteMason (toId attrs) Nothing
      pure l
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lead <- getLead
      enemiesToMove <-
        selectList $
          UnengagedEnemy
            <> ReadyEnemy
            <> EnemyWithTrait Witch
            <> NotEnemy (EnemyAt $ LocationWithId $ toId attrs)

      unless (null enemiesToMove) $
        push $
          chooseOneAtATime
            lead
            [ targetLabel
              enemy
              [MoveToward (toTarget enemy) (LocationWithId $ toId attrs)]
            | enemy <- enemiesToMove
            ]
      pure l
    _ -> WitchesCircle <$> runMessage msg attrs
