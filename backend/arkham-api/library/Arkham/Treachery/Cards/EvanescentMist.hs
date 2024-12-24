module Arkham.Treachery.Cards.EvanescentMist (evanescentMist) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EvanescentMist = EvanescentMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evanescentMist :: TreacheryCard EvanescentMist
evanescentMist = treachery EvanescentMist Cards.evanescentMist

instance RunMessage EvanescentMist where
  runMessage msg t@(EvanescentMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ NearestLocationTo iid (not_ LocationClearedOfMirages)
      chooseOrRunOneM iid $ targets locations \location -> do
        place attrs location
        placeClues attrs location 2
      pure t
    _ -> EvanescentMist <$> liftRunMessage msg attrs
