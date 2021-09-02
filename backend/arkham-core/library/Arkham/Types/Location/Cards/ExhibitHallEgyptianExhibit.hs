module Arkham.Types.Location.Cards.ExhibitHallEgyptianExhibit
  ( exhibitHallEgyptianExhibit
  , ExhibitHallEgyptianExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallEgyptianExhibit)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype ExhibitHallEgyptianExhibit = ExhibitHallEgyptianExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
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

instance HasAbilities ExhibitHallEgyptianExhibit where
  getAbilities (ExhibitHallEgyptianExhibit x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ SkillTestResult
          Timing.After
          You
          (WhileInvestigating $ LocationWithId $ toId x)
      $ FailureResult AnyValue
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env ExhibitHallEgyptianExhibit where
  runMessage msg l@(ExhibitHallEgyptianExhibit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (LoseActions iid (toSource attrs) 1)
    _ -> ExhibitHallEgyptianExhibit <$> runMessage msg attrs
