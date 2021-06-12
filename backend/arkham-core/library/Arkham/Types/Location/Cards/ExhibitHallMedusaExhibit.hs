module Arkham.Types.Location.Cards.ExhibitHallMedusaExhibit
  ( exhibitHallMedusaExhibit
  , ExhibitHallMedusaExhibit(..)
  )
where

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

newtype ExhibitHallMedusaExhibit = ExhibitHallMedusaExhibit LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedusaExhibit :: LocationId -> ExhibitHallMedusaExhibit
exhibitHallMedusaExhibit =
  ExhibitHallMedusaExhibit . (victoryL ?~ 1) . baseAttrs
    "02133"
    ("Exhibit Hall" `subtitled` "Medusa Exhibit")
    EncounterSet.TheMiskatonicMuseum
    2
    (PerPlayer 1)
    T
    [Square, Moon]
    [Miskatonic, Exhibit]

instance HasModifiersFor env ExhibitHallMedusaExhibit where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ExhibitHallMedusaExhibit where
  getActions iid window (ExhibitHallMedusaExhibit attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallMedusaExhibit where
  runMessage msg l@(ExhibitHallMedusaExhibit attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ unshiftMessage (ChooseAndDiscardAsset iid)
    _ -> ExhibitHallMedusaExhibit <$> runMessage msg attrs
