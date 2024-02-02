module Arkham.Treachery.Cards.ThroughTheGates (
  throughTheGates,
  ThroughTheGates (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ThroughTheGates = ThroughTheGates TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

throughTheGates :: TreacheryCard ThroughTheGates
throughTheGates = treachery ThroughTheGates Cards.throughTheGates

instance RunMessage ThroughTheGates where
  runMessage msg t@(ThroughTheGates attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      (mcard, _) <- fieldMap InvestigatorDeck (drawCard . unDeck) iid
      for_ mcard $ \(toCard -> card) -> do
        if card `cardMatch` WeaknessCard
          then pushAll [ObtainCard card, AddToHand iid [card]]
          else do
            send $ format (toCard attrs) <> " removed all copies of " <> format card <> " from the game"
            pushAll [ObtainCard card, RemoveAllCopiesOfCardFromGame iid (toCardCode card)]
      pure t
    _ -> ThroughTheGates <$> runMessage msg attrs
