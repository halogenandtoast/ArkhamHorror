module Arkham.Types.Location.Cards.ExhibitHallAthabaskanExhibit
  ( exhibitHallAthabaskanExhibit
  , ExhibitHallAthabaskanExhibit(..)
  ) where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ExhibitHallAthabaskanExhibit = ExhibitHallAthabaskanExhibit LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallAthabaskanExhibit :: ExhibitHallAthabaskanExhibit
exhibitHallAthabaskanExhibit = ExhibitHallAthabaskanExhibit $ baseAttrs
  "02132"
  (Name "Exhibit Hall" $ Just "Athabaskan Exhibit")
  EncounterSet.TheMiskatonicMuseum
  1
  (Static 0)
  Plus
  [Square]
  [Miskatonic, Exhibit]

instance HasModifiersFor env ExhibitHallAthabaskanExhibit where
  getModifiersFor _ (InvestigatorTarget iid) (ExhibitHallAthabaskanExhibit attrs)
    = pure $ toModifiers attrs [ SkillModifier SkillAgility 2 | iid `on` attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ExhibitHallAthabaskanExhibit where
  getActions iid window (ExhibitHallAthabaskanExhibit attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallAthabaskanExhibit where
  runMessage msg l@(ExhibitHallAthabaskanExhibit attrs) = case msg of
    AfterEnterLocation iid lid | lid == locationId attrs ->
      l <$ unshiftMessages
        [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]
    _ -> ExhibitHallAthabaskanExhibit <$> runMessage msg attrs
