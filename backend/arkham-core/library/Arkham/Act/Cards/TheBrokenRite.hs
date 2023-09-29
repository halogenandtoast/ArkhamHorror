module Arkham.Act.Cards.TheBrokenRite (
  TheBrokenRite (..),
  theBrokenRite,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Timing qualified as Timing

newtype TheBrokenRite = TheBrokenRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBrokenRite :: ActCard TheBrokenRite
theBrokenRite = act (4, A) TheBrokenRite Cards.theBrokenRite Nothing

instance HasAbilities TheBrokenRite where
  getAbilities (TheBrokenRite x)
    | onSide A x =
        [ restrictedAbility x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
        , mkAbility x 2
            $ Objective
            $ ReactionAbility
              ( Matcher.EnemyDefeated Timing.When Anyone ByAny
                  $ EnemyAt (locationIs Locations.theGeistTrap <> LocationWithBrazier Unlit)
                  <> enemyIs Enemies.theSpectralWatcher
              )
              Free
        ]
  getAbilities _ = []

instance RunMessage TheBrokenRite where
  runMessage msg a@(TheBrokenRite attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toSource attrs) SkillTestTarget (Difficulty (-2))
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push R4
      pure a
    _ -> TheBrokenRite <$> runMessage msg attrs
