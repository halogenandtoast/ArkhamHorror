module Arkham.Types.Event.Cards.HidingSpot
  ( hidingSpot
  , HidingSpot(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Keyword
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype HidingSpot = HidingSpot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hidingSpot :: EventCard HidingSpot
hidingSpot = event HidingSpot Cards.hidingSpot

instance Query EnemyMatcher env => HasModifiersFor env HidingSpot where
  getModifiersFor _ (EnemyTarget eid) (HidingSpot attrs) =
    case eventAttachedTarget attrs of
      Just (LocationTarget lid) -> do
        enemies <- select $ EnemyAt $ LocationWithId lid
        pure $ toModifiers attrs [ AddKeyword Aloof | eid `member` enemies ]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities env HidingSpot where
  getAbilities _ _ (HidingSpot x) = pure
    [ restrictedAbility
        x
        1
        (EnemyCriteria $ EnemyExistsAtAttachedLocation AnyEnemy)
      $ ForcedAbility
      $ PhaseEnds Timing.When
      $ PhaseIs EnemyPhase
    ]

instance EventRunner env => RunMessage env HidingSpot where
  runMessage msg e@(HidingSpot attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      targets <- selectListMap LocationTarget Anywhere
      e <$ push
        (chooseOne
          iid
          [ TargetLabel target [AttachEvent eid target] | target <- targets ]
        )
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    _ -> HidingSpot <$> runMessage msg attrs
