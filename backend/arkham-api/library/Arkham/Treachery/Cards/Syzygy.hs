module Arkham.Treachery.Cards.Syzygy (syzygy, Syzygy (..)) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Syzygy = Syzygy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

syzygy :: TreacheryCard Syzygy
syzygy = treachery Syzygy Cards.syzygy

instance RunMessage Syzygy where
  runMessage msg t@(Syzygy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid do
        whenAny InvestigatorWithAnyResources do
          labeled "Each investigator loses 3 resources." do
            eachInvestigator \iid' -> push $ LoseResources iid' (toSource attrs) 3
        labeled "Each investigator takes 2 horror." do
          eachInvestigator \iid' -> assignHorror iid' attrs 2
        labeled
          "Place 1 doom on the current agenda (this effect can cause the current agenda to advance)."
          $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> Syzygy <$> liftRunMessage msg attrs
