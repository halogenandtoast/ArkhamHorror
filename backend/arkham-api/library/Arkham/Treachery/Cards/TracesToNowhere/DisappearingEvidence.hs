module Arkham.Treachery.Cards.TracesToNowhere.DisappearingEvidence (disappearingEvidence) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DisappearingEvidence = DisappearingEvidence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disappearingEvidence :: TreacheryCard DisappearingEvidence
disappearingEvidence = treachery DisappearingEvidence Cards.disappearingEvidence

instance RunMessage DisappearingEvidence where
  runMessage msg t@(DisappearingEvidence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ #ally <> AssetAt (locationWithInvestigator iid)
      chooseOrRunOneM iid $ scenarioI18n do
        labeled "disappearingEvidence.discard" $ randomDiscard iid attrs
        labeledValidate (notNull allies) "disappearingEvidence.allyHorror" do
          chooseTargetM iid allies \ally -> dealAssetHorror ally attrs 1
      pure t
    _ -> DisappearingEvidence <$> liftRunMessage msg attrs
