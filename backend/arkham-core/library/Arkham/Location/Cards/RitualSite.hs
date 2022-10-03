module Arkham.Location.Cards.RitualSite where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( ritualSite )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype RitualSite = RitualSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualSite :: LocationCard RitualSite
ritualSite = location RitualSite Cards.ritualSite 3 (PerPlayer 2)

instance HasAbilities RitualSite where
  getAbilities (RitualSite attrs) | locationRevealed attrs =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 (CluesOnThis $ LessThan $ PerPlayer 2)
          $ ForcedAbility
          $ RoundEnds Timing.When
        ]
  getAbilities (RitualSite attrs) = getAbilities attrs

instance RunMessage RitualSite where
  runMessage msg l@(RitualSite attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      cluesToAdd <-
        max 0 . subtract (locationClues attrs) <$> getPlayerCountValue
          (PerPlayer 2)
      l <$ push (PlaceClues (toTarget attrs) cluesToAdd)
    _ -> RitualSite <$> runMessage msg attrs
