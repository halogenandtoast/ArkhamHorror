module Arkham.Location.Cards.IsolatedRoad (isolatedRoad) where

import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Trait (Trait (Hazard))
import Arkham.Treachery.Import.Lifted (gainSurge)

newtype IsolatedRoad = IsolatedRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

isolatedRoad :: LocationCard IsolatedRoad
isolatedRoad = location IsolatedRoad Cards.isolatedRoad 3 (PerPlayer 2)

instance RunMessage IsolatedRoad where
  runMessage msg l@(IsolatedRoad attrs) = runQueueT $ case msg of
    BeginRound -> pure $ IsolatedRoad $ attrs & setMeta True
    Revelation iid (TreacherySource tid) -> runDefaultMaybeT l do
      loc <- MaybeT $ getLocationOf iid
      guard $ loc == attrs.id
      guard $ getLocationMetaDefault True attrs
      card <- lift $ fetchCard tid
      guard $ Hazard `member` card.printedTraits
      lift do
        priority $ gainSurge tid
        pure $ IsolatedRoad $ attrs & setMeta False
    _ -> IsolatedRoad <$> liftRunMessage msg attrs
