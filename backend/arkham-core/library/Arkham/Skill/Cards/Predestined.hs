module Arkham.Skill.Cards.Predestined (predestined, Predestined (..)) where

import Arkham.Classes
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Predestined = Predestined SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predestined :: SkillCard Predestined
predestined = skill Predestined Cards.predestined

instance RunMessage Predestined where
  runMessage msg s@(Predestined attrs) = case msg of
    FailedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      n <- min 2 <$> getRemainingBlessTokens
      x <- selectCount $ ChaosTokenFaceIs #curse

      when (n > 0 || x > 0) do
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ Label ("Add " <> pluralize n "{bless} tokens") $ replicate n (AddChaosToken #bless)
            , Label ("Remove " <> pluralize x "{curse} tokens") $ replicate x (RemoveChaosToken #curse)
            ]

      pure s
    _ -> Predestined <$> runMessage msg attrs
