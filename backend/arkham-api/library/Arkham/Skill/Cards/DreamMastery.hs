module Arkham.Skill.Cards.DreamMastery (dreamMastery) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Token

newtype DreamMastery = DreamMastery SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamMastery :: SkillCard DreamMastery
dreamMastery = skill DreamMastery Cards.dreamMastery

instance RunMessage DreamMastery where
  runMessage msg s@(DreamMastery attrs) = runQueueT $ case msg of
    -- "your location" is the COMMITTER's location, not the performer's, so this
    -- routes off attrs.owner. `affectsOthersKnown` names the owner rather than
    -- reading the active seat (#5685/#6013).
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      let iid = attrs.owner
      dreaming <-
        select
          $ DreamingCardControlledBy (affectsOthersKnown iid $ at_ (locationWithInvestigator iid))
          <> DreamingCardWithDreams (atLeast 1)
      chooseTargetM iid dreaming \dc -> removeTokens attrs dc Dream 1
      pure s
    _ -> DreamMastery <$> liftRunMessage msg attrs
