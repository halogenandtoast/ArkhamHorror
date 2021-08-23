module Arkham.Types.Location.Cards.StudyAberrantGateway
  ( StudyAberrantGateway(..)
  , studyAberrantGateway
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (studyAberrantGateway)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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

instance ActionRunner env => HasAbilities env StudyAberrantGateway where
  getAbilities iid window@(Window Timing.When NonFast) (StudyAberrantGateway attrs)
    = withBaseAbilities iid window attrs $ do
      leadInvestigatorId <- getLeadInvestigatorId
      pure
        [ locationAbility
            (mkAbility attrs 1 $ ActionAbility Nothing $ ActionCost 2)
        | leadInvestigatorId == iid
        ]
  getAbilities iid window (StudyAberrantGateway attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env StudyAberrantGateway where
  runMessage msg l@(StudyAberrantGateway attrs@LocationAttrs {..}) =
    case msg of
      UseCardAbility iid (LocationSource lid) _ 1 _ | lid == locationId ->
        l <$ push (DrawCards iid 3 False)
      When (EnemySpawnAtLocationMatching _ locationMatcher _) -> do
        inPlay <- isJust <$> getId @(Maybe LocationId) locationMatcher
        l <$ unless inPlay (push (PlaceLocationMatching locationMatcher))
      _ -> StudyAberrantGateway <$> runMessage msg attrs
