module Arkham.Types.Location.Cards.ExhibitHallNatureExhibit
  ( exhibitHallNatureExhibit
  , ExhibitHallNatureExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Trait

newtype ExhibitHallNatureExhibit = ExhibitHallNatureExhibit LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallNatureExhibit :: LocationId -> ExhibitHallNatureExhibit
exhibitHallNatureExhibit lid = ExhibitHallNatureExhibit
  $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    lid
    "02134"
    (Name "Exhibit Hall" $ Just "Nature Exhibit")
    EncounterSet.TheMiskatonicMuseum
    4
    (PerPlayer 1)
    Hourglass
    [Square, Squiggle]
    [Miskatonic, Exhibit]

instance HasModifiersFor env ExhibitHallNatureExhibit where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ExhibitHallNatureExhibit where
  getActions iid window (ExhibitHallNatureExhibit attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallNatureExhibit where
  runMessage msg l@(ExhibitHallNatureExhibit attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l
      <$ unshiftMessages [RandomDiscard iid, RandomDiscard iid]
    _ -> ExhibitHallNatureExhibit <$> runMessage msg attrs
