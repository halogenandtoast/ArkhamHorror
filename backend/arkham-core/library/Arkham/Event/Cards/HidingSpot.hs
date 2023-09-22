module Arkham.Event.Cards.HidingSpot (
  hidingSpot,
  HidingSpot (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement

newtype HidingSpot = HidingSpot EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hidingSpot :: EventCard HidingSpot
hidingSpot = event HidingSpot Cards.hidingSpot

instance HasModifiersFor HidingSpot where
  getModifiersFor (EnemyTarget eid) (HidingSpot attrs) =
    case eventAttachedTarget attrs of
      Just (LocationTarget lid) -> do
        enemies <- select $ EnemyAt $ LocationWithId lid
        pure $ toModifiers attrs [AddKeyword Aloof | eid `member` enemies]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities HidingSpot where
  getAbilities (HidingSpot x) =
    [ restrictedAbility x 1 (EnemyCriteria $ EnemyExistsAtAttachedLocation AnyEnemy)
        $ ForcedAbility
        $ PhaseEnds #when #enemy
    ]

instance RunMessage HidingSpot where
  runMessage msg e@(HidingSpot attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      locations <- selectList Anywhere
      push $ chooseOne iid $ targetLabels locations (only . PlaceEvent iid eid . AttachedToLocation)
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Discard (toAbilitySource attrs 1) (toTarget attrs)
      pure e
    _ -> HidingSpot <$> runMessage msg attrs
