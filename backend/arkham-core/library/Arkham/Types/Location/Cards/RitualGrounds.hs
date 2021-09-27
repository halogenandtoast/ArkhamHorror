module Arkham.Types.Location.Cards.RitualGrounds where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (ritualGrounds)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype RitualGrounds = RitualGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ritualGrounds :: LocationCard RitualGrounds
ritualGrounds = location
  RitualGrounds
  Cards.ritualGrounds
  2
  (PerPlayer 1)
  Equals
  [Hourglass, Equals]

instance HasAbilities RitualGrounds where
  getAbilities (RitualGrounds attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ForcedAbility $ TurnEnds
          Timing.After
          You
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env RitualGrounds where
  runMessage msg l@(RitualGrounds attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      l <$ pushAll
        [ DrawCards iid 1 False
        , InvestigatorAssignDamage iid source DamageAny 0 1
        ]
    _ -> RitualGrounds <$> runMessage msg attrs
