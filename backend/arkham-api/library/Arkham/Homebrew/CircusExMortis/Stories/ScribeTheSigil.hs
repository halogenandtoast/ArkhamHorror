module Arkham.Homebrew.CircusExMortis.Stories.ScribeTheSigil (scribeTheSigil) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

{- | Destiny "sigil" (:205). "Flip this card over and put it into play" -- the
back is the Marked Grove location (:205b). 'otherSideIs' makes that def
single-sided, so it enters play already revealed: the location face IS the revealed face
of the physical card, and no 'RevealLocation' ever fires for it.
-}
newtype ScribeTheSigil = ScribeTheSigil StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scribeTheSigil :: StoryCard ScribeTheSigil
scribeTheSigil = story ScribeTheSigil Cards.scribeTheSigil

instance RunMessage ScribeTheSigil where
  runMessage msg s@(ScribeTheSigil attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      placeLocation_ Locations.markedGrove
      pure s
    _ -> ScribeTheSigil <$> liftRunMessage msg attrs
