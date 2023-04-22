module Arkham.Location.Cards.ExhibitHallEgyptianExhibit
  ( exhibitHallEgyptianExhibit
  , ExhibitHallEgyptianExhibit(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( exhibitHallEgyptianExhibit )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ExhibitHallEgyptianExhibit = ExhibitHallEgyptianExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallEgyptianExhibit :: LocationCard ExhibitHallEgyptianExhibit
exhibitHallEgyptianExhibit = location
  ExhibitHallEgyptianExhibit
  Cards.exhibitHallEgyptianExhibit
  3
  (PerPlayer 2)

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

instance RunMessage ExhibitHallEgyptianExhibit where
  runMessage msg l@(ExhibitHallEgyptianExhibit attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (LoseActions iid (toSource attrs) 1)
    _ -> ExhibitHallEgyptianExhibit <$> runMessage msg attrs
