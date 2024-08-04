module Arkham.Treachery.Cards.AbandonedAndAloneAdvanced where

import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AbandonedAndAloneAdvanced = AbandonedAndAloneAdvanced TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedAndAloneAdvanced :: TreacheryCard AbandonedAndAloneAdvanced
abandonedAndAloneAdvanced = treachery AbandonedAndAloneAdvanced Cards.abandonedAndAloneAdvanced

instance RunMessage AbandonedAndAloneAdvanced where
  runMessage msg t@(AbandonedAndAloneAdvanced attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discard <- field InvestigatorDiscard iid
      let nonWeaknesses = filter (`cardMatch` (not_ WeaknessCard)) discard
      for_ nonWeaknesses obtainCard

      if null nonWeaknesses
        then shuffleIntoDeck iid attrs
        else directHorror iid attrs 3

      pure t
    _ -> AbandonedAndAloneAdvanced <$> liftRunMessage msg attrs
