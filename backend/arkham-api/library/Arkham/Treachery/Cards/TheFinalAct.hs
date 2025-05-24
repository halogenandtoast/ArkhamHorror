module Arkham.Treachery.Cards.TheFinalAct (theFinalAct) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheFinalAct = TheFinalAct TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalAct :: TreacheryCard TheFinalAct
theFinalAct = treachery TheFinalAct Cards.theFinalAct

instance RunMessage TheFinalAct where
  runMessage msg t@(TheFinalAct attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      noRemainingSanity <- fieldP InvestigatorRemainingSanity (== 0) iid
      when noRemainingSanity $ placeDoomOnAgendaAndCheckAdvance 2
      pure t
    _ -> TheFinalAct <$> liftRunMessage msg attrs
