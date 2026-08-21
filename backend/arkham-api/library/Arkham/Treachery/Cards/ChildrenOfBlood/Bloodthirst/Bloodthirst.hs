module Arkham.Treachery.Cards.ChildrenOfBlood.Bloodthirst.Bloodthirst (bloodthirst) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Bloodthirst qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Bloodthirst = Bloodthirst TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodthirst :: TreacheryCard Bloodthirst
bloodthirst = treachery Bloodthirst Cards.bloodthirst

-- Sealing is deferred to the end of the test: SealChaosToken strips the token
-- from the skill test's revealed tokens, so doing it on reveal pulls the token
-- out from under the test's own resolution.
sealRevealedBlood :: ReverseQueue m => InvestigatorId -> m ()
sealRevealedBlood iid = do
  sealed <- selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
  blood <- filter ((== #blood) . (.face)) <$> getSkillTestRevealedChaosTokens
  for_ (take (3 - sealed) blood) $ sealChaosToken iid iid

instance RunMessage Bloodthirst where
  runMessage msg t@(Bloodthirst attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      sealRevealedBlood iid
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      sealRevealedBlood iid
      randomDiscard iid attrs
      pure t
    _ -> Bloodthirst <$> liftRunMessage msg attrs
