module Arkham.Treachery.Cards.Amnesia where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Projection
import Arkham.Investigator.Types
import Arkham.Treachery.Runner

newtype Amnesia = Amnesia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amnesia :: TreacheryCard Amnesia
amnesia = treachery Amnesia Cards.amnesia

instance RunMessage Amnesia where
  runMessage msg t@(Amnesia attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cardCount' <- fieldMap InvestigatorHand length iid
      t <$ pushAll (replicate (cardCount' - 1) (ChooseAndDiscardCard iid))
    _ -> Amnesia <$> runMessage msg attrs
