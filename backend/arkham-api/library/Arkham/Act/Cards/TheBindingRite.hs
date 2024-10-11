module Arkham.Act.Cards.TheBindingRite (TheBindingRite (..), theBindingRite) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Modifier
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype TheBindingRite = TheBindingRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBindingRite :: ActCard TheBindingRite
theBindingRite = act (4, A) TheBindingRite Cards.theBindingRite Nothing

instance HasAbilities TheBindingRite where
  getAbilities (TheBindingRite x)
    | onSide A x =
        [ restricted x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
        , mkAbility x 2
            $ Objective
            $ freeReaction
            $ Matcher.EnemyDefeated #when Anyone ByAny
            $ at_ (locationIs Locations.theGeistTrap <> LocationWithBrazier Lit)
            <> enemyIs Enemies.theSpectralWatcher
        ]
  getAbilities _ = []

instance RunMessage TheBindingRite where
  runMessage msg a@(TheBindingRite attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push R1
      pure a
    _ -> TheBindingRite <$> liftRunMessage msg attrs
