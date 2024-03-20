module Arkham.Treachery.Cards.WhisperingChaosSouth (
  whisperingChaosSouth,
  WhisperingChaosSouth (..),
)
where

import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhisperingChaosSouth = WhisperingChaosSouth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperingChaosSouth :: TreacheryCard WhisperingChaosSouth
whisperingChaosSouth = treachery WhisperingChaosSouth Cards.whisperingChaosSouth

instance RunMessage WhisperingChaosSouth where
  runMessage msg t@(WhisperingChaosSouth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceTreachery (toId attrs) (TreacheryInHandOf iid)
      pure t
    _ -> WhisperingChaosSouth <$> lift (runMessage msg attrs)
