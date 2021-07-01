module Arkham.Types.Location.Cards.ExhibitHallEgyptianExhibit
  ( exhibitHallEgyptianExhibit
  , ExhibitHallEgyptianExhibit(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallEgyptianExhibit)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ExhibitHallEgyptianExhibit = ExhibitHallEgyptianExhibit LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallEgyptianExhibit :: LocationId -> ExhibitHallEgyptianExhibit
exhibitHallEgyptianExhibit =
  ExhibitHallEgyptianExhibit . baseAttrs
    Cards.exhibitHallEgyptianExhibit
    3
    (PerPlayer 2)
    Moon
    [Square, T]

instance HasModifiersFor env ExhibitHallEgyptianExhibit where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ExhibitHallEgyptianExhibit where
  getActions iid window (ExhibitHallEgyptianExhibit attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallEgyptianExhibit where
  runMessage msg l@(ExhibitHallEgyptianExhibit attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l
      <$ unshiftMessage (LoseActions iid (toSource attrs) 1)
    _ -> ExhibitHallEgyptianExhibit <$> runMessage msg attrs
