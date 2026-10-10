module Arkham.Homebrew.AgesUnwound.Treacheries.HigherPowers (higherPowers) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype HigherPowers = HigherPowers TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherPowers :: TreacheryCard HigherPowers
higherPowers = treachery HigherPowers Cards.higherPowers

{- | "__Revelation__ - Attach Exposition to San Francisco. Place 1 resource on
each of Mexico City, Istanbul and Shanghai. / __Task__ - Expose the operations
of the Myriad."

/Grudging Assistance/ completes this Task.
-}
instance RunMessage HigherPowers where
  runMessage msg t@(HigherPowers attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      selectForMaybeM (locationIs Locations.sanFrancisco)
        $ createTreacheryAt_ Cards.exposition
        . AttachedToLocation
      for_ [Locations.mexicoCity, Locations.istanbul, Locations.shanghai] \def ->
        selectForMaybeM (locationIs def) \lid -> placeTokens attrs lid #resource 1
      pure t
    _ -> HigherPowers <$> liftRunMessage msg attrs
