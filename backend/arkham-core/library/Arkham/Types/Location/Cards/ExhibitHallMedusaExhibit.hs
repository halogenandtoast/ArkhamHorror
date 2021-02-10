module Arkham.Types.Location.Cards.ExhibitHallMedusaExhibit
  ( exhibitHallMedusaExhibit
  , ExhibitHallMedusaExhibit(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import qualified Arkham.Types.Action as Action
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ExhibitHallMedusaExhibit = ExhibitHallMedusaExhibit LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedusaExhibit :: ExhibitHallMedusaExhibit
exhibitHallMedusaExhibit = ExhibitHallMedusaExhibit
  $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "02133"
    (Name "Exhibit Hall" $ Just "Medusa Exhibit")
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
