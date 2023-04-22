module Arkham.Location.Cards.FacultyOfficesTheNightIsStillYoung
  ( facultyOfficesTheNightIsStillYoung
  , FacultyOfficesTheNightIsStillYoung(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
  ( facultyOfficesTheNightIsStillYoung )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype FacultyOfficesTheNightIsStillYoung = FacultyOfficesTheNightIsStillYoung LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facultyOfficesTheNightIsStillYoung
  :: LocationCard FacultyOfficesTheNightIsStillYoung
facultyOfficesTheNightIsStillYoung = location
  FacultyOfficesTheNightIsStillYoung
  Cards.facultyOfficesTheNightIsStillYoung
  2
  (PerPlayer 2)

instance HasModifiersFor FacultyOfficesTheNightIsStillYoung where
  getModifiersFor target (FacultyOfficesTheNightIsStillYoung attrs)
    | isTarget attrs target = pure
    $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ = pure []

instance HasAbilities FacultyOfficesTheNightIsStillYoung where
  getAbilities (FacultyOfficesTheNightIsStillYoung x) =
    withBaseAbilities x $ if locationRevealed x
      then
        [ mkAbility x 1
        $ ForcedAbility
        $ RevealLocation Timing.After Anyone
        $ LocationWithId
        $ toId x
        , restrictedAbility x 2 Here $ FastAbility $ GroupClueCost
          (PerPlayer 2)
          (LocationWithTitle "Faculty Offices")
        ]
      else []

instance RunMessage FacultyOfficesTheNightIsStillYoung where
  runMessage msg l@(FacultyOfficesTheNightIsStillYoung attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ FindEncounterCard
            iid
            (toTarget attrs)
            [FromEncounterDeck, FromEncounterDiscard]
        $ CardWithType EnemyType
        <> CardWithTrait Humanoid
      pure l
    FoundEncounterCard _iid target card | isTarget attrs target ->
      l <$ push (SpawnEnemyAt (EncounterCard card) (toId attrs))
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      l <$ push (ScenarioResolution $ Resolution 1)
    _ -> FacultyOfficesTheNightIsStillYoung <$> runMessage msg attrs
