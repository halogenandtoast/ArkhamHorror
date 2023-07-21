module Arkham.Treachery.Cards.Amnesia where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Amnesia = Amnesia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

amnesia :: TreacheryCard Amnesia
amnesia = treachery Amnesia Cards.amnesia

instance RunMessage Amnesia where
  runMessage msg t@(Amnesia attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      cardCount' <- fieldMap InvestigatorHand length iid
      pushAll $
        replicate (cardCount' - 1) $
          toMessage $
            chooseAndDiscardCard
              iid
              attrs
      pure t
    _ -> Amnesia <$> runMessage msg attrs
