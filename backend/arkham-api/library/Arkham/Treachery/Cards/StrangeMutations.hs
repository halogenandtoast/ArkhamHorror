module Arkham.Treachery.Cards.StrangeMutations (strangeMutations) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StrangeMutations = StrangeMutations TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeMutations :: TreacheryCard StrangeMutations
strangeMutations = treachery StrangeMutations Cards.strangeMutations

instance RunMessage StrangeMutations where
  runMessage msg t@(StrangeMutations attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SubtractCalculation (Fixed 5) (InvestigatorFieldCalculation iid InvestigatorHorror)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      handSize <- length <$> field InvestigatorHand iid
      chooseOrRunOneM iid $ withI18n do
        countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        when (handSize >= 2) $ countVar 2 $ labeled' "discardRandomCardsFromHand" $ randomDiscardN iid attrs 2
      pure t
    _ -> StrangeMutations <$> liftRunMessage msg attrs
