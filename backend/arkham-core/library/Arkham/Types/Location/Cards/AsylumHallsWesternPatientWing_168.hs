module Arkham.Types.Location.Cards.AsylumHallsWesternPatientWing_168
  ( asylumHallsWesternPatientWing_168
  , AsylumHallsWesternPatientWing_168(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyDefeated)
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait

newtype AsylumHallsWesternPatientWing_168 = AsylumHallsWesternPatientWing_168 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsWesternPatientWing_168
  :: LocationCard AsylumHallsWesternPatientWing_168
asylumHallsWesternPatientWing_168 = location
  AsylumHallsWesternPatientWing_168
  Cards.asylumHallsWesternPatientWing_168
  2
  (Static 0)
  Circle
  [Hourglass, Triangle, Diamond]

instance HasAbilities AsylumHallsWesternPatientWing_168 where
  getAbilities (AsylumHallsWesternPatientWing_168 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
        $ ReactionAbility
            (EnemyDefeated Timing.After You $ EnemyWithTrait Lunatic)
            Free
    ]

instance LocationRunner env => RunMessage env AsylumHallsWesternPatientWing_168 where
  runMessage msg l@(AsylumHallsWesternPatientWing_168 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 1 False)
    _ -> AsylumHallsWesternPatientWing_168 <$> runMessage msg attrs
