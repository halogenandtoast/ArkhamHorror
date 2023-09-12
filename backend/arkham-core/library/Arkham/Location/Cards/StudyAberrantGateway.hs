module Arkham.Location.Cards.StudyAberrantGateway (
  StudyAberrantGateway (..),
  studyAberrantGateway,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (studyAberrantGateway)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype StudyAberrantGateway = StudyAberrantGateway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studyAberrantGateway :: LocationCard StudyAberrantGateway
studyAberrantGateway =
  location StudyAberrantGateway Cards.studyAberrantGateway 3 (PerPlayer 1)

instance HasAbilities StudyAberrantGateway where
  getAbilities (StudyAberrantGateway attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 2
        , mkAbility attrs 2 $ ForcedAbility $ EnemyAttemptsToSpawnAt Timing.When AnyEnemy LocationNotInPlay
        ]

getMatcher :: [Window] -> LocationMatcher
getMatcher [] = error "Expected a window"
getMatcher ((windowType -> Window.EnemyAttemptsToSpawnAt _ matcher) : _) = matcher
getMatcher (_ : rest) = getMatcher rest

instance RunMessage StudyAberrantGateway where
  runMessage msg l@(StudyAberrantGateway attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 3
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 (getMatcher -> matcher) _ -> do
      case matcher of
        LocationWithTitle title -> push (PlaceLocationMatching $ CardWithTitle title)
        _ -> error "Expected everything to use titles"
      pure l
    _ -> StudyAberrantGateway <$> runMessage msg attrs
