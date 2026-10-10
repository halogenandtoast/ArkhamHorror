module Arkham.Homebrew.AgesUnwound.Enemies.DeterminedGeneral (determinedGeneral) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (CannotBeDamaged), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers (spawnRomanSoldier)
import Arkham.Matcher

newtype DeterminedGeneral = DeterminedGeneral EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /For the Glory of Rome!/ -- one of the four [[Elite]] enemies setup shuffles
together. Alert and Retaliate are on the card def.
-}
determinedGeneral :: EnemyCard DeterminedGeneral
determinedGeneral = enemy DeterminedGeneral Cards.determinedGeneral

{- | "Determined General cannot be damaged while there is a ready Roman Soldier
enemy at its location."

The soldiers are minted from the top card of a player's deck but are real
enemies built from the @:ages-unwound:900@ def, so they match on card code
rather than on title.
-}
instance HasModifiersFor DeterminedGeneral where
  getModifiersFor (DeterminedGeneral a) = do
    hasGuard <-
      selectAny
        $ enemyIs Cards.romanSoldier
        <> ReadyEnemy
        <> EnemyAt (locationWithEnemy a.id)
    when hasGuard $ modifySelf a [CannotBeDamaged]

{- | "Forced - After Determined General engages or attacks you: Put the top card
of your deck into play in your threat area, as a Roman Soldier enemy with 3
fight, 1 health, 3 evade, 1 damage and the [[Humanoid]] trait."
-}
instance HasAbilities DeterminedGeneral where
  getAbilities (DeterminedGeneral a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)
      , mkAbility a 2 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      ]

instance RunMessage DeterminedGeneral where
  runMessage msg e@(DeterminedGeneral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) n | n `elem` [1, 2] -> do
      spawnRomanSoldier iid
      pure e
    _ -> DeterminedGeneral <$> liftRunMessage msg attrs
