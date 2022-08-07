module Arkham.Location.Cards.AsylumHallsWesternPatientWing_168
  ( asylumHallsWesternPatientWing_168
  , AsylumHallsWesternPatientWing_168(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype AsylumHallsWesternPatientWing_168 = AsylumHallsWesternPatientWing_168 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsWesternPatientWing_168
  :: LocationCard AsylumHallsWesternPatientWing_168
asylumHallsWesternPatientWing_168 = location
  AsylumHallsWesternPatientWing_168
  Cards.asylumHallsWesternPatientWing_168
  2
  (Static 0)

instance HasAbilities AsylumHallsWesternPatientWing_168 where
  getAbilities (AsylumHallsWesternPatientWing_168 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
        $ ReactionAbility
            (EnemyDefeated Timing.After You $ EnemyWithTrait Lunatic)
            Free
    ]

instance RunMessage AsylumHallsWesternPatientWing_168 where
  runMessage msg l@(AsylumHallsWesternPatientWing_168 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 1 False)
    _ -> AsylumHallsWesternPatientWing_168 <$> runMessage msg attrs
