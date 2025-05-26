module Arkham.Treachery.Cards.NeedForKnowledge (needForKnowledge) where

import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NeedForKnowledge = NeedForKnowledge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

needForKnowledge :: TreacheryCard NeedForKnowledge
needForKnowledge = treachery NeedForKnowledge Cards.needForKnowledge

instance RunMessage NeedForKnowledge where
  runMessage msg t@(NeedForKnowledge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      clues <- min 5 <$> iid.clues
      if clues == 0
        then gainSurge attrs
        else do
          sid <- getRandom
          revelationSkillTest sid iid attrs #intellect (Fixed clues)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> withI18n do
      clues <- iid.clues
      chooseOrRunOneM iid do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
        when (clues > 0) do
          countVar 1 $ labeled' "placeCluesOnYourLocation" $ placeCluesOnLocation iid attrs 1
      pure t
    _ -> NeedForKnowledge <$> liftRunMessage msg attrs
