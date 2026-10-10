module Arkham.Homebrew.AgesUnwound.Treacheries.TheyJustKeepComing (theyJustKeepComing) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype TheyJustKeepComing = TheyJustKeepComing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyJustKeepComing :: TreacheryCard TheyJustKeepComing
theyJustKeepComing = treachery TheyJustKeepComing Cards.theyJustKeepComing

{- | "Revelation - Spawn a copy of The Myriad Gentleman in each of the following
locations, if they are revealed: the Entrance Hall, the Landing and the Study. If
no enemies are spawned this way, They Just Keep Coming gains surge."

The card does not say whose deck the copies come from, so the guide's tiebreak
applies and the lead investigator supplies them.
-}
instance RunMessage TheyJustKeepComing where
  runMessage msg t@(TheyJustKeepComing attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      locations <-
        select
          $ RevealedLocation
          <> mapOneOf locationIs [Locations.entranceHall, Locations.landing, Locations.study]
      if null locations
        then gainSurge attrs
        else do
          lead <- getLead
          for_ locations $ spawnMyriadCopiesAt lead 1
      pure t
    _ -> TheyJustKeepComing <$> liftRunMessage msg attrs
