module Arkham.Location.Cards.FacultyOfficesTheNightIsStillYoung (
  facultyOfficesTheNightIsStillYoung,
  FacultyOfficesTheNightIsStillYoung (..),
) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype FacultyOfficesTheNightIsStillYoung = FacultyOfficesTheNightIsStillYoung LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facultyOfficesTheNightIsStillYoung
  :: LocationCard FacultyOfficesTheNightIsStillYoung
facultyOfficesTheNightIsStillYoung =
  location
    FacultyOfficesTheNightIsStillYoung
    Cards.facultyOfficesTheNightIsStillYoung
    2
    (PerPlayer 2)

instance HasModifiersFor FacultyOfficesTheNightIsStillYoung where
  getModifiersFor (FacultyOfficesTheNightIsStillYoung a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities FacultyOfficesTheNightIsStillYoung where
  getAbilities (FacultyOfficesTheNightIsStillYoung x) =
    extendRevealed
      x
      [ mkAbility x 1 $ forced $ RevealLocation #after Anyone (be x)
      , restrictedAbility x 2 Here
          $ FastAbility
          $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Faculty Offices")
      ]

instance RunMessage FacultyOfficesTheNightIsStillYoung where
  runMessage msg l@(FacultyOfficesTheNightIsStillYoung attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs $ #enemy <> CardWithTrait Humanoid
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      spawnEnemyAt_ card attrs
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R1
      pure l
    _ -> FacultyOfficesTheNightIsStillYoung <$> liftRunMessage msg attrs
