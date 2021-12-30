module Arkham.Event.Cards.HidingSpot
  ( hidingSpot
  , HidingSpot(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Criteria
import Arkham.Event.Attrs
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Target
import Arkham.Timing qualified as Timing

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

instance HasAbilities HidingSpot where
  getAbilities (HidingSpot x) =
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
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <- selectListMap LocationTarget Anywhere
      e <$ push
        (chooseOne
          iid
          [ TargetLabel target [AttachEvent eid target] | target <- targets ]
        )
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    _ -> HidingSpot <$> runMessage msg attrs
