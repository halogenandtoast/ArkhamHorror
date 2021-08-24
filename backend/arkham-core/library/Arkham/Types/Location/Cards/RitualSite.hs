module Arkham.Types.Location.Cards.RitualSite where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (ritualSite)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype RitualSite = RitualSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSite :: LocationCard RitualSite
ritualSite =
  location RitualSite Cards.ritualSite 3 (PerPlayer 2) Plus [Squiggle]

instance HasAbilities env RitualSite where
  getAbilities i window (RitualSite attrs) | locationRevealed attrs =
    withBaseAbilities i window attrs $ pure
      [ restrictedAbility attrs 1 (CluesOnThis $ LessThan $ PerPlayer 2)
        $ ForcedAbility
        $ RoundEnds Timing.When
      ]
  getAbilities i window (RitualSite attrs) = getAbilities i window attrs

instance LocationRunner env => RunMessage env RitualSite where
  runMessage msg l@(RitualSite attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      cluesToAdd <-
        max 0 . subtract (locationClues attrs) <$> getPlayerCountValue
          (PerPlayer 2)
      l <$ push (PlaceClues (toTarget attrs) cluesToAdd)
    _ -> RitualSite <$> runMessage msg attrs
