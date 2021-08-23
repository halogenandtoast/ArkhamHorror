module Arkham.Types.Location.Cards.ExhibitHallEgyptianExhibit
  ( exhibitHallEgyptianExhibit
  , ExhibitHallEgyptianExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallEgyptianExhibit)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message

newtype ExhibitHallEgyptianExhibit = ExhibitHallEgyptianExhibit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallEgyptianExhibit :: LocationCard ExhibitHallEgyptianExhibit
exhibitHallEgyptianExhibit = locationWithRevealedSideConnections
  ExhibitHallEgyptianExhibit
  Cards.exhibitHallEgyptianExhibit
  3
  (PerPlayer 2)
  NoSymbol
  [Square]
  Moon
  [Square, T]

instance HasModifiersFor env ExhibitHallEgyptianExhibit

instance HasAbilities env ExhibitHallEgyptianExhibit where
  getAbilities iid window (ExhibitHallEgyptianExhibit attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallEgyptianExhibit where
  runMessage msg l@(ExhibitHallEgyptianExhibit attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ push (LoseActions iid (toSource attrs) 1)
    _ -> ExhibitHallEgyptianExhibit <$> runMessage msg attrs
