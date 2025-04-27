module Arkham.Treachery.Cards.TheZealotsSeal (theZealotsSeal) where

import Arkham.Helpers.Investigator (getHandCount)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheZealotsSeal = TheZealotsSeal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theZealotsSeal :: TreacheryCard TheZealotsSeal
theZealotsSeal = treachery TheZealotsSeal Cards.theZealotsSeal

instance RunMessage TheZealotsSeal where
  runMessage msg t@(TheZealotsSeal attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      forEachInvestigator $ doStep 1 msg
      forEachInvestigator $ doStep 2 msg
      pure t
    ForInvestigator iid (DoStep 1 (Revelation _ (isSource attrs -> True))) -> do
      handCardCount <- getHandCount iid
      when (handCardCount <= 3) $ assignDamageAndHorror iid attrs 1 1
      pure t
    ForInvestigator iid (DoStep 2 (Revelation _ (isSource attrs -> True))) -> do
      handCardCount <- getHandCount iid
      when (handCardCount >= 4) do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      randomDiscardN iid attrs 2
      pure t
    _ -> TheZealotsSeal <$> liftRunMessage msg attrs
