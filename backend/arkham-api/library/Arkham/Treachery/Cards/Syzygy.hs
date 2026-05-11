module Arkham.Treachery.Cards.Syzygy (syzygy, Syzygy (..)) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (campaignI18n)
import Arkham.I18n
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
      chooseOneM iid $ campaignI18n $ scope "syzygy" do
        whenAny InvestigatorWithAnyResources do
          labeled' "loseResources" do
            eachInvestigator \iid' -> push $ LoseResources iid' (toSource attrs) 3
        labeled' "takeHorror" do
          eachInvestigator \iid' -> assignHorror iid' attrs 2
        labeled' "placeDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
      pure t
    _ -> Syzygy <$> liftRunMessage msg attrs
