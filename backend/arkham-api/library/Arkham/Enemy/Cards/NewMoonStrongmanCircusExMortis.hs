module Arkham.Enemy.Cards.NewMoonStrongmanCircusExMortis (newMoonStrongmanCircusExMortis) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestAction, getSkillTestTargetedEnemy)
import Arkham.Keyword qualified as Keyword

newtype NewMoonStrongmanCircusExMortis = NewMoonStrongmanCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

newMoonStrongmanCircusExMortis :: EnemyCard NewMoonStrongmanCircusExMortis
newMoonStrongmanCircusExMortis = enemy NewMoonStrongmanCircusExMortis Cards.newMoonStrongmanCircusExMortis

instance HasModifiersFor NewMoonStrongmanCircusExMortis where
  getModifiersFor (NewMoonStrongmanCircusExMortis a) = do
    modifySelf a [AddKeyword Keyword.Hunter]
    -- "Each [combat] icon committed to attacks against New Moon Strongman counts as 2
    -- matching icons instead. (Does not double [wild].)"
    -- TODO(homebrew): DoubleSkillIcons doubles ALL of a committed card's icons, including
    -- [wild]. There is no per-SkillType icon-doubling primitive, so a card committed with
    -- both [combat] and [wild] over-counts its [wild] by 1 here. See report: engine gap.
    getSkillTest >>= traverse_ \st -> do
      action <- getSkillTestAction
      menemy <- getSkillTestTargetedEnemy
      when (action == Just #fight && menemy == Just a.id) do
        modifyEach a (concat $ toList st.committedCards) [DoubleSkillIcons]

instance RunMessage NewMoonStrongmanCircusExMortis where
  runMessage msg (NewMoonStrongmanCircusExMortis attrs) =
    NewMoonStrongmanCircusExMortis <$> runMessage msg attrs
