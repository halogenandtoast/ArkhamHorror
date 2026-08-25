module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonStrongman (newMoonStrongman) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestAction, getSkillTestTargetedEnemy)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword

newtype NewMoonStrongman = NewMoonStrongman EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonStrongman :: EnemyCard NewMoonStrongman
newMoonStrongman = enemy NewMoonStrongman Cards.newMoonStrongman

instance HasModifiersFor NewMoonStrongman where
  getModifiersFor (NewMoonStrongman a) = do
    modifySelf a [AddKeyword Keyword.Hunter]
    getSkillTest >>= traverse_ \st -> do
      action <- getSkillTestAction
      menemy <- getSkillTestTargetedEnemy
      when (action == Just #fight && menemy == Just a.id) do
        modifyEach a (concat $ toList st.committedCards) [DoubleSkillIconsOf [#combat]]

instance RunMessage NewMoonStrongman where
  runMessage msg (NewMoonStrongman attrs) =
    NewMoonStrongman <$> runMessage msg attrs
