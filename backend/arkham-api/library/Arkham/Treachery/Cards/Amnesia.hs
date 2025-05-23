module Arkham.Treachery.Cards.Amnesia (amnesia) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Amnesia = Amnesia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amnesia :: TreacheryCard Amnesia
amnesia = treachery Amnesia Cards.amnesia

instance RunMessage Amnesia where
  runMessage msg t@(Amnesia attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hand <- fieldMap InvestigatorHand length iid
      when (hand > 1) $ chooseAndDiscardCards iid attrs (hand - 1)
      pure t
    _ -> Amnesia <$> liftRunMessage msg attrs
