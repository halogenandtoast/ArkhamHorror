module Arkham.Skill.Cards.Providential2 (providential2, Providential2 (..)) where

import Arkham.Helpers.ChaosBag (getRemainingBlessTokens)
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted hiding (InvestigatorDamage)

newtype Providential2 = Providential2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

providential2 :: SkillCard Providential2
providential2 = skill Providential2 Cards.providential2

instance RunMessage Providential2 where
  runMessage msg s@(Providential2 attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      n <- getRemainingBlessTokens
      d <- field InvestigatorDamage attrs.owner
      h <- field InvestigatorHorror attrs.owner
      let x = min n (min d h)
      replicateM_ x $ push $ AddChaosToken #bless
      pure s
    _ -> Providential2 <$> liftRunMessage msg attrs
