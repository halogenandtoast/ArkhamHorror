module Arkham.Homebrew.AgesUnwound.Stories.ColourOutOfSpace (colourOutOfSpace) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype ColourOutOfSpace = ColourOutOfSpace StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /The Tunguska Event/ (@:ages-unwound:154@).
colourOutOfSpace :: StoryCard ColourOutOfSpace
colourOutOfSpace = story ColourOutOfSpace Cards.colourOutOfSpace

{- | "Put the set-aside Brainwashed Expedition enemy into play at Tunguska. Flip
this card back over. For the remainder of the game, it cannot be flipped over
again."

Flipping back is a no-op -- a treachery has no flipped face in the engine, the
story card is what the players saw -- and the lock lives on the treachery, whose
ability becomes 'Never' once it has been used.
-}
instance RunMessage ColourOutOfSpace where
  runMessage msg s@(ColourOutOfSpace attrs) = runQueueT $ case msg of
    ResolveThisStory _iid (is attrs -> True) -> do
      createSetAsideEnemy_ Enemies.brainwashedExpedition (locationIs Locations.tunguska)
      pure s
    _ -> ColourOutOfSpace <$> liftRunMessage msg attrs
