module Arkham.Types.Location.Cards.ExhibitHallNatureExhibit
  ( exhibitHallNatureExhibit
  , ExhibitHallNatureExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallNatureExhibit)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

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

instance HasAbilities env ExhibitHallNatureExhibit where
  getAbilities i w (ExhibitHallNatureExhibit x) = withBaseAbilities i w x $ do
    pure
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
