module Arkham.Location.Cards.RitualGrounds where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (ritualGrounds)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype RitualGrounds = RitualGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ritualGrounds :: LocationCard RitualGrounds
ritualGrounds = location RitualGrounds Cards.ritualGrounds 2 (PerPlayer 1)

instance HasAbilities RitualGrounds where
  getAbilities (RitualGrounds attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 Here
            $ ForcedAbility
            $ TurnEnds Timing.After You
        ]

instance RunMessage RitualGrounds where
  runMessage msg l@(RitualGrounds attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawing <- drawCards iid (toAbilitySource attrs 1) 1
      pushAll [drawing, assignHorror iid (toAbilitySource attrs 1) 1]
      pure l
    _ -> RitualGrounds <$> runMessage msg attrs
