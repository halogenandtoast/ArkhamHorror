module Arkham.Treachery.Cards.HarvestedPain (harvestedPain) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HarvestedPain = HarvestedPain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harvestedPain :: TreacheryCard HarvestedPain
harvestedPain = treachery HarvestedPain Cards.harvestedPain

-- While Harvested Pain is in the victory display, Eixodolon gets +1 fight
-- (implemented on Eixodolon) and readies at the start of each enemy phase.
instance RunMessage HarvestedPain where
  runMessage msg t@(HarvestedPain attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      health <- field InvestigatorRemainingHealth iid
      sanity <- field InvestigatorRemainingSanity iid
      if health <= 3 || sanity <= 3
        then addToVictory iid attrs
        else assignDamageAndHorror iid attrs 1 1
      pure t
    _ -> HarvestedPain <$> liftRunMessage msg attrs
