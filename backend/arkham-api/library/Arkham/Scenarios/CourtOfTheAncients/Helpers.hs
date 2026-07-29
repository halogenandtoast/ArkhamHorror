module Arkham.Scenarios.CourtOfTheAncients.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Card (cardMatch)
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Direction (GridDirection (GridLeft, GridRight))
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid (updatePosition)
import Arkham.Location.Types (Field (LocationPosition), LocationAttrs, locationPosition)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing
import Arkham.Trait (Trait (Glyph))

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
