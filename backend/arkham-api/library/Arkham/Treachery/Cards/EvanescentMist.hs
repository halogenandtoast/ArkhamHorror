module Arkham.Treachery.Cards.EvanescentMist (evanescentMist) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EvanescentMist = EvanescentMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evanescentMist :: TreacheryCard EvanescentMist
evanescentMist = treachery EvanescentMist Cards.evanescentMist

instance HasAbilities EvanescentMist where
  getAbilities (EvanescentMist a) =
    [restricted a 1 (exists $ locationWithTreachery a <> LocationClearedOfMirages) $ forced AnyWindow]

instance RunMessage EvanescentMist where
  runMessage msg t@(EvanescentMist attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Rest is handled by the @mirageRunner@
      locations <- select $ NearestLocationTo iid (not_ LocationClearedOfMirages)
      chooseOrRunOneM iid $ targets locations \location -> do
        place attrs location
        placeClues attrs location 2
      pure t
    _ -> EvanescentMist <$> liftRunMessage msg attrs
