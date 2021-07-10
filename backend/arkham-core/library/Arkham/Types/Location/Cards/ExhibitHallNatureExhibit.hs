module Arkham.Types.Location.Cards.ExhibitHallNatureExhibit
  ( exhibitHallNatureExhibit
  , ExhibitHallNatureExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallNatureExhibit)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message

newtype ExhibitHallNatureExhibit = ExhibitHallNatureExhibit LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallNatureExhibit :: LocationCard ExhibitHallNatureExhibit
exhibitHallNatureExhibit = location
  ExhibitHallNatureExhibit
  Cards.exhibitHallNatureExhibit
  4
  (PerPlayer 1)
  Hourglass
  [Square, Squiggle]

instance HasModifiersFor env ExhibitHallNatureExhibit

instance ActionRunner env => HasActions env ExhibitHallNatureExhibit where
  getActions iid window (ExhibitHallNatureExhibit attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallNatureExhibit where
  runMessage msg l@(ExhibitHallNatureExhibit attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l
      <$ pushAll [RandomDiscard iid, RandomDiscard iid]
    _ -> ExhibitHallNatureExhibit <$> runMessage msg attrs
