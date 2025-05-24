module Arkham.Event.Events.HidingSpot (hidingSpot) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Keyword
import Arkham.Matcher

newtype HidingSpot = HidingSpot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hidingSpot :: EventCard HidingSpot
hidingSpot = event HidingSpot Cards.hidingSpot

instance HasModifiersFor HidingSpot where
  getModifiersFor (HidingSpot attrs) =
    case attrs.attachedTo.location of
      Just lid -> modifySelect attrs (enemyAt lid) [AddKeyword Aloof]
      _ -> pure ()

instance HasAbilities HidingSpot where
  getAbilities (HidingSpot x) =
    [ restricted x 1 (EnemyCriteria $ EnemyExistsAtAttachedLocation AnyEnemy)
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage HidingSpot where
  runMessage msg e@(HidingSpot attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseSelectM iid Anywhere $ place attrs . AttachedToLocation
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> HidingSpot <$> liftRunMessage msg attrs
