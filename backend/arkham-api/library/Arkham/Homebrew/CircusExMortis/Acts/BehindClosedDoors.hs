module Arkham.Homebrew.CircusExMortis.Acts.BehindClosedDoors (behindClosedDoors) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Restricted))

newtype BehindClosedDoors = BehindClosedDoors ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

behindClosedDoors :: ActCard BehindClosedDoors
behindClosedDoors = act (1, A) BehindClosedDoors Cards.behindClosedDoors (groupClueCost $ PerPlayer 7)

instance RunMessage BehindClosedDoors where
  runMessage msg a@(BehindClosedDoors attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocationsMatching_ (CardWithTrait Restricted)
      advanceActDeck attrs
      pure a
    _ -> BehindClosedDoors <$> liftRunMessage msg attrs
