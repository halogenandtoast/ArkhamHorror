module Arkham.Event.Events.HidingSpot (
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
import Arkham.Placement

newtype HidingSpot = HidingSpot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hidingSpot :: EventCard HidingSpot
hidingSpot = event HidingSpot Cards.hidingSpot

instance HasModifiersFor HidingSpot where
  getModifiersFor (HidingSpot attrs) =
    case eventAttachedTarget attrs of
      Just (LocationTarget lid) -> modifySelect attrs (EnemyAt $ LocationWithId lid) [AddKeyword Aloof]
      _ -> pure mempty

instance HasAbilities HidingSpot where
  getAbilities (HidingSpot x) =
    [ restrictedAbility x 1 (EnemyCriteria $ EnemyExistsAtAttachedLocation AnyEnemy)
        $ ForcedAbility
        $ PhaseEnds #when #enemy
    ]

instance RunMessage HidingSpot where
  runMessage msg e@(HidingSpot attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      locations <- select Anywhere
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels locations (only . PlaceEvent eid . AttachedToLocation)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure e
    _ -> HidingSpot <$> runMessage msg attrs
