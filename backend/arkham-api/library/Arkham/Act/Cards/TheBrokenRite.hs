module Arkham.Act.Cards.TheBrokenRite (theBrokenRite) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Brazier
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.UnionAndDisillusion.Helpers

newtype TheBrokenRite = TheBrokenRite ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBrokenRite :: ActCard TheBrokenRite
theBrokenRite = act (4, A) TheBrokenRite Cards.theBrokenRite Nothing

instance HasAbilities TheBrokenRite where
  getAbilities = actAbilities \x ->
    [ restricted x 1 DuringCircleAction $ FastAbility $ ClueCost (Static 1)
    , mkAbility x 2
        $ Objective
        $ freeReaction
        $ ifEnemyDefeatedMatch
        $ at_ (locationIs Locations.theGeistTrap <> LocationWithBrazier Unlit)
        <> enemyIs Enemies.theSpectralWatcher
    ]

instance RunMessage TheBrokenRite where
  runMessage msg a@(TheBrokenRite attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R4
      pure a
    _ -> TheBrokenRite <$> liftRunMessage msg attrs
