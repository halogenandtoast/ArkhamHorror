module Arkham.Location.Cards.RitualSite where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (ritualSite)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype RitualSite = RitualSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ritualSite :: LocationCard RitualSite
ritualSite = location RitualSite Cards.ritualSite 3 (PerPlayer 2)

instance HasAbilities RitualSite where
  getAbilities (RitualSite attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 (CluesOnThis $ LessThan $ PerPlayer 2)
            $ ForcedAbility
            $ RoundEnds Timing.When
        ]

instance RunMessage RitualSite where
  runMessage msg l@(RitualSite attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      cluesToAdd <- max 0 . subtract (locationClues attrs) <$> perPlayer 2
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) cluesToAdd
      pure l
    _ -> RitualSite <$> runMessage msg attrs
