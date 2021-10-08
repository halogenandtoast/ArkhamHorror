module Arkham.Types.Location.Cards.AsylumHallsEasternPatientWing_170
  ( asylumHallsEasternPatientWing_170
  , AsylumHallsEasternPatientWing_170(..)
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
import Arkham.Types.Matcher hiding (EnemyEvaded)
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype AsylumHallsEasternPatientWing_170 = AsylumHallsEasternPatientWing_170 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

asylumHallsEasternPatientWing_170
  :: LocationCard AsylumHallsEasternPatientWing_170
asylumHallsEasternPatientWing_170 = location
  AsylumHallsEasternPatientWing_170
  Cards.asylumHallsEasternPatientWing_170
  3
  (Static 0)
  Hourglass
  [Circle, Heart, Squiggle]

instance HasAbilities AsylumHallsEasternPatientWing_170 where
  getAbilities (AsylumHallsEasternPatientWing_170 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        (Here <> EnemyCriteria
          (EnemyExists
          $ EnemyAt (LocationWithId $ toId attrs)
          <> EnemyWithTrait Lunatic
          )
        )
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, HorrorCost (toSource attrs) YouTarget 1]
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env AsylumHallsEasternPatientWing_170 where
  runMessage msg l@(AsylumHallsEasternPatientWing_170 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectList
        (EnemyAt (LocationWithId $ toId attrs) <> EnemyWithTrait Lunatic)
      l <$ push
        (chooseOne
          iid
          [ TargetLabel (EnemyTarget eid) [EnemyEvaded iid eid]
          | eid <- enemies
          ]
        )
    _ -> AsylumHallsEasternPatientWing_170 <$> runMessage msg attrs
