module Arkham.Types.Location.Cards.ExhibitHallMedusaExhibit
  ( exhibitHallMedusaExhibit
  , ExhibitHallMedusaExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallMedusaExhibit)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype ExhibitHallMedusaExhibit = ExhibitHallMedusaExhibit LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedusaExhibit :: LocationCard ExhibitHallMedusaExhibit
exhibitHallMedusaExhibit = locationWithRevealedSideConnections
  ExhibitHallMedusaExhibit
  Cards.exhibitHallMedusaExhibit
  2
  (PerPlayer 1)
  NoSymbol
  [Square]
  T
  [Square, Moon]

instance HasModifiersFor env ExhibitHallMedusaExhibit

instance ActionRunner env => HasAbilities env ExhibitHallMedusaExhibit where
  getAbilities iid window (ExhibitHallMedusaExhibit attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env ExhibitHallMedusaExhibit where
  runMessage msg l@(ExhibitHallMedusaExhibit attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ target _ _)
      | isTarget attrs target -> l <$ push (ChooseAndDiscardAsset iid AnyAsset)
    _ -> ExhibitHallMedusaExhibit <$> runMessage msg attrs
