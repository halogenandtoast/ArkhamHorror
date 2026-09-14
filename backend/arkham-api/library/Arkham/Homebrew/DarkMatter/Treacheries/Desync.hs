module Arkham.Homebrew.DarkMatter.Treacheries.Desync (desync) where

import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories, campaignI18n)
import Arkham.Investigator.Types (Field (InvestigatorDeck))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype Desync = Desync TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desync :: TreacheryCard Desync
desync = treachery Desync Cards.desync

instance RunMessage Desync where
  runMessage msg t@(Desync attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      deckSize <- fieldMap InvestigatorDeck length iid
      chooseOneM iid $ campaignI18n do
        labeled "desync.traumaAndExile" do
          sufferMentalTrauma iid 1
          addMemories iid 1
          exile attrs
          removeCardFromDeckForCampaign iid (toCard attrs)
        labeled "desync.returnToDeck"
          $ if deckSize >= 5
            then shuffleIntoDeck iid attrs
            else toDiscard attrs attrs
      pure t
    _ -> Desync <$> liftRunMessage msg attrs
