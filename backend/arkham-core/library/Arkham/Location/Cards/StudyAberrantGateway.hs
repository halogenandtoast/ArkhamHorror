module Arkham.Location.Cards.StudyAberrantGateway
  ( StudyAberrantGateway(..)
  , studyAberrantGateway
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( studyAberrantGateway )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype StudyAberrantGateway = StudyAberrantGateway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studyAberrantGateway :: LocationCard StudyAberrantGateway
studyAberrantGateway =
  location StudyAberrantGateway Cards.studyAberrantGateway 3 (PerPlayer 1)

instance HasAbilities StudyAberrantGateway where
  getAbilities (StudyAberrantGateway attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 2
    , mkAbility attrs 2 $ ForcedAbility $ EnemyAttemptsToSpawnAt
      Timing.When
      AnyEnemy
      LocationNotInPlay
    ]

instance RunMessage StudyAberrantGateway where
  runMessage msg l@(StudyAberrantGateway attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 3 False)
    UseCardAbility _ source [Window _ (Window.EnemyAttemptsToSpawnAt _ locationMatcher)] 2 _
      | isSource attrs source
      -> do
        case locationMatcher of
          LocationWithTitle title ->
            l <$ push (PlaceLocationMatching $ CardWithTitle title)
          _ -> error "Expected everything to use titles"
    _ -> StudyAberrantGateway <$> runMessage msg attrs
