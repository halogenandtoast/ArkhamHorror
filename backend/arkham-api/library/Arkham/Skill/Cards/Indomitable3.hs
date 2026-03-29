module Arkham.Skill.Cards.Indomitable3 (indomitable3) where

import {-# SOURCE #-} Arkham.GameEnv (getHistoryField)
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.History
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Indomitable3 = Indomitable3 SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

indomitable3 :: SkillCard Indomitable3
indomitable3 = skill Indomitable3 Cards.indomitable3

instance HasModifiersFor Indomitable3 where
  getModifiersFor (Indomitable3 a) = do
    location <- getJustLocation a.owner
    investigators <- select $ investigatorAt location
    enemyAttackedThisRound <- anyM
      ( \iid -> do
          enemies <- getHistoryField #round iid HistoryEnemiesAttackedBy
          pure $ not $ null enemies
      )
      investigators
    addSkillIconsWhen a enemyAttackedThisRound [#wild, #wild, #wild]

instance RunMessage Indomitable3 where
  runMessage msg (Indomitable3 attrs) = Indomitable3 <$> runMessage msg attrs
