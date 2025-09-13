module Arkham.Skill.Cards.Predestined (predestined) where

import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Predestined = Predestined SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predestined :: SkillCard Predestined
predestined = skill Predestined Cards.predestined

instance RunMessage Predestined where
  runMessage msg s@(Predestined attrs) = runQueueT $ case msg of
    FailedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      chooseOrRunOneM iid do
        n <- min 2 <$> getRemainingBlessTokens
        when (n > 0) do
          labeled ("Add " <> pluralize n "{bless} token") $ repeated n (addChaosToken #bless)

        x <- selectCount $ ChaosTokenFaceIs #curse
        when (x > 0) do
          labeled ("Remove " <> pluralize x "{curse} token") $ repeated x (removeChaosToken #curse)

      pure s
    _ -> Predestined <$> liftRunMessage msg attrs
