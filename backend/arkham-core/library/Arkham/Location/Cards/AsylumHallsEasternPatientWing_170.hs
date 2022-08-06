module Arkham.Location.Cards.AsylumHallsEasternPatientWing_170
  ( asylumHallsEasternPatientWing_170
  , AsylumHallsEasternPatientWing_170(..)
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
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype AsylumHallsEasternPatientWing_170 = AsylumHallsEasternPatientWing_170 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_170
  :: LocationCard AsylumHallsEasternPatientWing_170
asylumHallsEasternPatientWing_170 = location
  AsylumHallsEasternPatientWing_170
  Cards.asylumHallsEasternPatientWing_170
  3
  (Static 0)

instance HasAbilities AsylumHallsEasternPatientWing_170 where
  getAbilities (AsylumHallsEasternPatientWing_170 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> EnemyCriteria
          (EnemyExists $ EnemyAt YourLocation <> EnemyWithTrait Lunatic)
        )
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, HorrorCost (toSource attrs) YouTarget 1]
    | locationRevealed attrs
    ]

instance RunMessage AsylumHallsEasternPatientWing_170 where
  runMessage msg l@(AsylumHallsEasternPatientWing_170 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectList (EnemyAt YourLocation <> EnemyWithTrait Lunatic)
      push $ chooseOne
        iid
        [ targetLabel eid [EnemyEvaded iid eid] | eid <- enemies ]
      pure l
    _ -> AsylumHallsEasternPatientWing_170 <$> runMessage msg attrs
