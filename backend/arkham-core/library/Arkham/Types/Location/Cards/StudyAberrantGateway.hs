module Arkham.Types.Location.Cards.StudyAberrantGateway
  ( StudyAberrantGateway(..)
  , studyAberrantGateway
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (studyAberrantGateway)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype StudyAberrantGateway = StudyAberrantGateway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

studyAberrantGateway :: LocationCard StudyAberrantGateway
studyAberrantGateway = location
  StudyAberrantGateway
  Cards.studyAberrantGateway
  3
  (PerPlayer 1)
  Circle
  [T]

instance HasAbilities StudyAberrantGateway where
  getAbilities (StudyAberrantGateway attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 2
    , mkAbility attrs 2 $ ForcedAbility $ EnemyAttemptsToSpawnAt
      Timing.When
      AnyEnemy
      LocationNotInPlay
    ]

instance LocationRunner env => RunMessage env StudyAberrantGateway where
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
