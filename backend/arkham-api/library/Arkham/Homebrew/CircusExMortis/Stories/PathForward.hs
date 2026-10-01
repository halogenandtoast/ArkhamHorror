{- | All four Path Forward copies (:178-:181) print the same card and differ only in
which column of the row the first sentence names, so they share this implementation.
Nothing here is an ability: the card is a pair of static conditions, and a facedown copy
contributes no modifiers at all.
-}
module Arkham.Homebrew.CircusExMortis.Stories.PathForward (
  besideRow,
  pathLabelRow,
  pathForwardModifiers,
) where

import Arkham.Classes.HasModifiersFor
import Arkham.Classes.Query
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_, modifyEach)
import Arkham.Homebrew.CircusExMortis.Helpers (RowEnd, locationAtRowEnd)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Story.Types
import Data.Text qualified as T

{- | The row the copy was placed beside. It sits in a grid cell of its own named
@path\<row\>@ ('AsSelfLocation'), which is "in play, but not at any location", so the
row is read straight back out of that label.
-}
besideRow :: StoryAttrs -> Maybe Int
besideRow a = case a.placement of
  AsSelfLocation label -> pathLabelRow label
  _ -> Nothing

-- | The row named by a @path\<row\>@ cell label. Act 1 reads it from the other side.
pathLabelRow :: Text -> Maybe Int
pathLabelRow label = readMay . unpack =<< T.stripPrefix "path" label

{- | @column@ is the one the first sentence names. The second sentence always names the
leftmost location of the above row, on all four copies.
-}
pathForwardModifiers :: HasModifiersM m => StoryAttrs -> RowEnd -> m ()
pathForwardModifiers a column = unless a.flipped do
  for_ (besideRow a) \y -> do
    above <- select $ LocationInRow (y + 1)
    unless (null above) do
      {- "If <column> is revealed and there are no clues on it, that location is
      connected to each location in the row above it." The printed symbols only connect a
      row downward, so this is the scenario's only way back up. It stays a one-way grant:
      the above row already connects down by symbol. Both halves of the condition are
      live, so a clue landing back on the column closes the path again. -}
      row <- select $ LocationInRow y
      locationAtRowEnd column row >>= traverse_ \col -> do
        open <- col <=~> (RevealedLocation <> LocationWithoutClues)
        modifiedWhen_ a open col [ConnectedToWhen (LocationWithId col) (mapOneOf LocationWithId above)]

      {- "The <column> location in the above row gains Victory 1. This victory applies
      even if there is only one location in the above row." The same column the first
      sentence names, counted in the row above rather than this one -- each copy says it
      of its own column, not of the leftmost. A separate sentence, so it is not under the
      first sentence's "if" — being faceup is the whole condition. Kept a live modifier
      rather than a one-shot push so it does not depend on the above row already being on
      the board when act 1 flips the card; 'getInitialVictory' scores revealed clueless
      locations in place, which reads it. -}
      locationAtRowEnd column above >>= traverse_ \lid -> do
        card <- field LocationCard lid
        modifyEach a [card] [GainVictory 1]
