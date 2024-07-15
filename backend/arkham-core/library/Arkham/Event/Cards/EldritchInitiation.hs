module Arkham.Event.Cards.EldritchInitiation (eldritchInitiation, EldritchInitiation (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Slot
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype EldritchInitiation = EldritchInitiation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchInitiation :: EventCard EldritchInitiation
eldritchInitiation = event EldritchInitiation Cards.eldritchInitiation

instance RunMessage EldritchInitiation where
  runMessage msg e@(EldritchInitiation attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      arcaneSlots <- fieldMap InvestigatorSlots (findWithDefault [] #arcane) iid
      drawCardsIfCan iid attrs (min 5 $ length arcaneSlots)
      doStep 1 msg
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      arcaneSlots <- fieldMap InvestigatorSlots (findWithDefault [] #arcane) iid
      let n = count (not . isEmptySlot) arcaneSlots
      discardFromHand iid attrs DiscardChoose n
      pure e
    _ -> EldritchInitiation <$> liftRunMessage msg attrs
