module Arkham.Event.Events.InTheShadows (inTheShadows, InTheShadows (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype InTheShadows = InTheShadows EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheShadows :: EventCard InTheShadows
inTheShadows = event InTheShadows Cards.inTheShadows

instance RunMessage InTheShadows where
  runMessage msg e@(InTheShadows attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      enemies <- select $ enemyEngagedWith iid
      for_ enemies $ disengageEnemy iid
      roundModifiers attrs iid [CannotBeEngaged, CannotDealDamage]
      pure e
    _ -> InTheShadows <$> liftRunMessage msg attrs
