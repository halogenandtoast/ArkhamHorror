module Arkham.Location.Cards.ExhibitHallNatureExhibit
  ( exhibitHallNatureExhibit
  , ExhibitHallNatureExhibit(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (exhibitHallNatureExhibit)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype ExhibitHallNatureExhibit = ExhibitHallNatureExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallNatureExhibit :: LocationCard ExhibitHallNatureExhibit
exhibitHallNatureExhibit = locationWithRevealedSideConnections
  ExhibitHallNatureExhibit
  Cards.exhibitHallNatureExhibit
  4
  (PerPlayer 1)
  NoSymbol
  [Square]
  Hourglass
  [Square, Squiggle]

instance HasAbilities ExhibitHallNatureExhibit where
  getAbilities (ExhibitHallNatureExhibit x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ Enters Timing.After You
      $ LocationWithId
      $ toId x
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env ExhibitHallNatureExhibit where
  runMessage msg l@(ExhibitHallNatureExhibit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll [RandomDiscard iid, RandomDiscard iid]
    _ -> ExhibitHallNatureExhibit <$> runMessage msg attrs
