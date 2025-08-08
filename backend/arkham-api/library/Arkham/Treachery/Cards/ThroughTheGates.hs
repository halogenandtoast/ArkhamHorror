module Arkham.Treachery.Cards.ThroughTheGates (throughTheGates) where

import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers qualified as Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThroughTheGates = ThroughTheGates TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheGates :: TreacheryCard ThroughTheGates
throughTheGates = treachery ThroughTheGates Cards.throughTheGates

instance RunMessage ThroughTheGates where
  runMessage msg t@(ThroughTheGates attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      (mcard, _) <- fieldMap InvestigatorDeck (Helpers.drawCard . unDeck) iid
      for_ mcard $ \(toCard -> card) -> do
        unless (card `cardMatch` WeaknessCard) do
          send $ format (toCard attrs) <> " removed all copies of " <> format card <> " from the game"
        focusCards [card] do
          prompt iid "Resolve Through the Gates" do
            if card `cardMatch` WeaknessCard
              then drawCard iid card
              else do
                pushAll [RemoveCard card.id, RemoveAllCopiesOfCardFromGame iid (toCardCode card)]
      pure t
    _ -> ThroughTheGates <$> liftRunMessage msg attrs
