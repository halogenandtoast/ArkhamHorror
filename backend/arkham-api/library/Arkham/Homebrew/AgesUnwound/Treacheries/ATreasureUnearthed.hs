module Arkham.Homebrew.AgesUnwound.Treacheries.ATreasureUnearthed (aTreasureUnearthed) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (placeThisTask)
import Arkham.Treachery.Import.Lifted

newtype ATreasureUnearthed = ATreasureUnearthed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTreasureUnearthed :: TreacheryCard ATreasureUnearthed
aTreasureUnearthed = treachery ATreasureUnearthed Cards.aTreasureUnearthed

{- | "__Revelation__ - Put the set-aside The British Library into play, revealed
location side faceup. / __Task__ - Steal a tome from the British Library."

The library's builder is already revealed, so placing it is the whole of
"revealed location side faceup". /Book Heist/ completes this Task.
-}
instance RunMessage ATreasureUnearthed where
  runMessage msg t@(ATreasureUnearthed attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeThisTask attrs
      placeSetAsideLocation_ Locations.theBritishLibrary
      pure t
    _ -> ATreasureUnearthed <$> liftRunMessage msg attrs
