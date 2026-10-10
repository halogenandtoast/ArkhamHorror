module Arkham.Homebrew.AgesUnwound.Enemies.HiredThugs (hiredThugs) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype HiredThugs = HiredThugs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiredThugs :: EnemyCard HiredThugs
hiredThugs = enemy HiredThugs Cards.hiredThugs

-- | "While Hired Thugs are engaged with you, you get -1 [agility]."
instance HasModifiersFor HiredThugs where
  getModifiersFor (HiredThugs a) =
    modifySelect a (investigatorEngagedWith a.id) [SkillModifier #agility (-1)]

instance RunMessage HiredThugs where
  runMessage msg (HiredThugs attrs) = HiredThugs <$> runMessage msg attrs
