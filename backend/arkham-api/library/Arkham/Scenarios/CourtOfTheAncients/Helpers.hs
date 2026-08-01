module Arkham.Scenarios.CourtOfTheAncients.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card (cardMatch)
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Direction
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid
import Arkham.Location.Types (Field (LocationPosition), LocationAttrs, locationPosition)
import Arkham.Matcher
import Arkham.Message (Message (PlaceGrid))
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing
import Arkham.Trait (Trait (Glyph, Lift))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "courtOfTheAncients" a

{- | The tower is a vertical grid: a location's "level" is its grid row + 1
(row 0 = level 1). Mirrors ToTheForbiddenPeaks/TheWesternWall: must read the
level via the investigator so the @Projection Location@ instance is visible.
-}
getLocationLevel
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m, Tracing m)
  => investigator -> m Int
getLocationLevel investigator =
  fromMaybe 0 <$> runMaybeT do
    loc <- MaybeT $ getMaybeLocation investigator
    pos <- MaybeT $ field LocationPosition loc
    pure (pos.row + 1)

{- | The number of [[Glyph]] cards currently in the victory display (drives the
skull token and several Court effects).
-}
getVictoryGlyphCount :: (HasGame m, Tracing m) => m Int
getVictoryGlyphCount = count (`cardMatch` CardWithTrait Glyph) <$> getVictoryDisplay

{- | "The Great Lift is only connected to the locations to the left and right of
it, and vice versa."

Both directions have to be declared: nothing in this scenario is connected by
grid adjacency alone, so without the reverse half an investigator standing on a
neighbouring location could never step onto the lift. Reading the lift's live
grid position (rather than a static @connectsTo@) is what makes the connections
follow it as it slides between levels — @PlaceGrid@ updates @locationPosition@,
so this recomputes on every slide.

Shared by both faces of the card so the connections can't drift apart when the
lift flips from (Inactive) to (Active).
-}
greatLiftConnections :: HasModifiersM m => LocationAttrs -> m ()
greatLiftConnections a = for_ (locationPosition a) \pos -> do
  let neighbors = map (updatePosition pos) [GridLeft, GridRight]
  modifySelf a [ConnectedToWhen (be a) (mapOneOf LocationInPosition neighbors)]
  for_ neighbors \neighbor ->
    modifySelect a (LocationInPosition neighbor) [ConnectedToWhen (LocationInPosition neighbor) (be a)]

{- | Slide the Great Lift down once: move the Great Lift location down one level
(toward level 1 = lower grid row), carrying all its cards/tokens/investigators
(they stay attached to the same LocationId, so @PlaceGrid@ preserves them).
The lift cannot slide below level 1 (row 0).
-}
slideGreatLiftDown :: ReverseQueue m => m ()
slideGreatLiftDown = do
  selectOne (LocationWithTrait Lift) >>= traverse_ \greatLift -> do
    field LocationPosition greatLift >>= traverse_ \pos ->
      when (positionRow pos > 0) do
        push $ PlaceGrid (GridLocation (updatePosition pos GridDown) greatLift)
