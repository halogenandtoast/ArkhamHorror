module Arkham.Homebrew.AgesUnwound.Treacheries.EntreatingTheGods (entreatingTheGods) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype EntreatingTheGods = EntreatingTheGods TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entreatingTheGods :: TreacheryCard EntreatingTheGods
entreatingTheGods = treachery EntreatingTheGods Cards.entreatingTheGods

{- | "__Revelation__ - Attach the set-aside Distant Entity asset to Sydney. /
__Task__ - Make contact with a distant entity."

/A Blessing from On High/ completes this Task.
-}
instance RunMessage EntreatingTheGods where
  runMessage msg t@(EntreatingTheGods attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      selectForMaybeM (locationIs Locations.sydney)
        $ createAssetAt_ Assets.distantEntity
        . AttachedToLocation
      pure t
    _ -> EntreatingTheGods <$> liftRunMessage msg attrs
