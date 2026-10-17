module Arkham.Homebrew.AgesUnwound.Enemies.HiredThugs (hiredThugs) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype HiredThugs = HiredThugs EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hiredThugs :: EnemyCard HiredThugs
hiredThugs = enemy HiredThugs Cards.hiredThugs

instance HasModifiersFor HiredThugs where
  getModifiersFor (HiredThugs a) = modifySelect a (investigatorEngagedWith a.id) [SkillModifier #agility (-1)]
