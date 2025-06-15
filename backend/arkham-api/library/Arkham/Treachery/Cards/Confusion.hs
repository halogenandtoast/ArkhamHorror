module Arkham.Treachery.Cards.Confusion (confusion) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Confusion = Confusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

confusion :: TreacheryCard Confusion
confusion = treachery Confusion Cards.confusion

instance RunMessage Confusion where
  runMessage msg t@(Confusion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      clues <- field InvestigatorClues iid
      let n = min 2 clues
          rest = 2 - n
      placeCluesOnLocation iid attrs n
      when (rest > 0) $ assignHorror iid attrs rest
      pure t
    _ -> Confusion <$> liftRunMessage msg attrs
