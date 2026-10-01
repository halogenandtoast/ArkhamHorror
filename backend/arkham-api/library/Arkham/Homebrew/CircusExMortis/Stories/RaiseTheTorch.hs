module Arkham.Homebrew.CircusExMortis.Stories.RaiseTheTorch (raiseTheTorch) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

{- | Destiny "torch" (:203). "Flip this card over and put it into play" -- the
back is the Forest Chasm location (:203b). 'otherSideIs' makes that def
single-sided, so it enters play already revealed: the location face IS the revealed face
of the physical card, and no 'RevealLocation' ever fires for it.
-}
newtype RaiseTheTorch = RaiseTheTorch StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

raiseTheTorch :: StoryCard RaiseTheTorch
raiseTheTorch = story RaiseTheTorch Cards.raiseTheTorch

instance RunMessage RaiseTheTorch where
  runMessage msg s@(RaiseTheTorch attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      placeLocation_ Locations.forestChasm
      pure s
    _ -> RaiseTheTorch <$> liftRunMessage msg attrs
