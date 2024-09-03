module Arkham.Event.Cards.InTheShadows (
  inTheShadows,
  InTheShadows (..),
)
where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype InTheShadows = InTheShadows EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheShadows :: EventCard InTheShadows
inTheShadows = event InTheShadows Cards.inTheShadows

instance RunMessage InTheShadows where
  runMessage msg e@(InTheShadows attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <- select $ enemyEngagedWith iid
      pushAll
        $ map (DisengageEnemy iid) enemies
        <> [roundModifiers attrs iid [CannotBeEngaged, CannotDealDamage]]
      pure e
    _ -> InTheShadows <$> runMessage msg attrs
