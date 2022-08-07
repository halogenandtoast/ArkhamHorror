module Arkham.Location.Cards.ScienceBuilding where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( scienceBuilding )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype ScienceBuilding = ScienceBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scienceBuilding :: LocationCard ScienceBuilding
scienceBuilding =
  location ScienceBuilding Cards.scienceBuilding 2 (PerPlayer 1)

instance HasAbilities ScienceBuilding where
  getAbilities (ScienceBuilding x) =
    withBaseAbilities x $ if locationRevealed x
      then
        [ restrictedAbility
          x
          1
          (Here <> Negate (LocationExists $ LocationWithTitle "Alchemy Labs"))
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId x
        , restrictedAbility x 2 Here $ ForcedAbility $ SkillTestResult
          Timing.When
          You
          (SkillTestWithSkillType SkillWillpower)
          (FailureResult AnyValue)
        ]
      else []

instance RunMessage ScienceBuilding where
  runMessage msg l@(ScienceBuilding attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (PlaceLocationMatching $ CardWithTitle "Alchemy Labs")
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> ScienceBuilding <$> runMessage msg attrs
