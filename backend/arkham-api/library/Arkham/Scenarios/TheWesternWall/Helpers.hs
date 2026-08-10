module Arkham.Scenarios.TheWesternWall.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (CannotEnter, SetShroud), modifySelect, modifySelf)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid (Pos)
import Arkham.Location.Types (Field (LocationPosition), LocationAttrs)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theWesternWall" a

-- Western Wall is row 0 and level 1 in both versions. V.I places higher
-- levels on negative rows (below it), while V.II places them on positive rows
-- (above it), so distance from row 0 determines the level in either layout.
locationLevel :: Pos -> Int
locationLevel pos = abs pos.row + 1

cannotEnterFromCluedLocation :: HasModifiersM m => LocationAttrs -> m ()
cannotEnterFromCluedLocation a =
  when a.unrevealed
    $ modifySelect a (InvestigatorAt $ LocationWithClues $ atLeast 1) [CannotEnter a.id]

treacherousPathModifiers :: HasModifiersM m => LocationAttrs -> m ()
treacherousPathModifiers a = do
  cannotEnterFromCluedLocation a
  for_ a.position \pos -> modifySelf a [SetShroud $ locationLevel pos]

getLocationLevel
  :: (AsId investigator, IdOf investigator ~ InvestigatorId, HasGame m)
  => investigator -> m Int
getLocationLevel investigator =
  fromMaybe 0 <$> runMaybeT do
    loc <- MaybeT $ getMaybeLocation investigator
    pos <- MaybeT $ field LocationPosition loc
    pure $ locationLevel pos
