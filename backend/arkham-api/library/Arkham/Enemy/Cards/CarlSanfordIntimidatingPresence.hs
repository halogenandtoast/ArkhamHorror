module Arkham.Enemy.Cards.CarlSanfordIntimidatingPresence (carlSanfordIntimidatingPresence) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype CarlSanfordIntimidatingPresence = CarlSanfordIntimidatingPresence EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

carlSanfordIntimidatingPresence :: EnemyCard CarlSanfordIntimidatingPresence
carlSanfordIntimidatingPresence =
  enemy CarlSanfordIntimidatingPresence Cards.carlSanfordIntimidatingPresence (3, Static 3, 3) (0, 1)

instance HasModifiersFor CarlSanfordIntimidatingPresence where
  getModifiersFor (CarlSanfordIntimidatingPresence a) = do
    modifySelf a [CannotBeDamaged]
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [SkillModifier #willpower (-1)]

instance RunMessage CarlSanfordIntimidatingPresence where
  runMessage msg (CarlSanfordIntimidatingPresence attrs) =
    CarlSanfordIntimidatingPresence <$> runMessage msg attrs
