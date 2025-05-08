module Arkham.Treachery.Cards.TwistedToHisWill (twistedToHisWill) where

import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TwistedToHisWill = TwistedToHisWill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twistedToHisWill :: TreacheryCard TwistedToHisWill
twistedToHisWill = treachery TwistedToHisWill Cards.twistedToHisWill

instance RunMessage TwistedToHisWill where
  runMessage msg t@(TwistedToHisWill attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      doomCount <- getDoomCount
      sid <- getRandom
      if doomCount > 0
        then revelationSkillTest sid iid attrs #willpower DoomCountCalculation
        else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      randomDiscardN iid attrs 2
      pure t
    _ -> TwistedToHisWill <$> liftRunMessage msg attrs
