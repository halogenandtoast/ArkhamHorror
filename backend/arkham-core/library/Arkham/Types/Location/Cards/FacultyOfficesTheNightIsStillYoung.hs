module Arkham.Types.Location.Cards.FacultyOfficesTheNightIsStillYoung
  ( facultyOfficesTheNightIsStillYoung
  , FacultyOfficesTheNightIsStillYoung(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
  (facultyOfficesTheNightIsStillYoung)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Modifier
import Arkham.Types.Resolution
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

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
  T
  [Circle]

instance HasModifiersFor env FacultyOfficesTheNightIsStillYoung where
  getModifiersFor _ target (FacultyOfficesTheNightIsStillYoung attrs)
    | isTarget attrs target = pure
    $ toModifiers attrs [ Blocked | not (locationRevealed attrs) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities env FacultyOfficesTheNightIsStillYoung where
  getAbilities iid window (FacultyOfficesTheNightIsStillYoung x) =
    withBaseAbilities iid window x $ pure $ if locationRevealed x
      then
        [ mkAbility x 1
        $ ForcedAbility
        $ RevealLocation Timing.After Anyone
        $ LocationWithId
        $ toId x
        , restrictedAbility x 2 Here $ FastAbility $ GroupClueCost
          (PerPlayer 2)
          (Just $ LocationWithTitle "Faculty Offices")
        ]
      else []

instance LocationRunner env => RunMessage env FacultyOfficesTheNightIsStillYoung where
  runMessage msg l@(FacultyOfficesTheNightIsStillYoung attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (FindEncounterCard iid (toTarget attrs)
      $ CardWithType EnemyType
      <> CardWithTrait Humanoid
      )
    FoundEncounterCard _iid target card | isTarget attrs target ->
      l <$ push (SpawnEnemyAt (EncounterCard card) (toId attrs))
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      l <$ push (ScenarioResolution $ Resolution 1)
    _ -> FacultyOfficesTheNightIsStillYoung <$> runMessage msg attrs
