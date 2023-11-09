module Arkham.Event.Cards.AGlimmerOfHope (
  aGlimmerOfHope,
  AGlimmerOfHope (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype AGlimmerOfHope = AGlimmerOfHope EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aGlimmerOfHope :: EventCard AGlimmerOfHope
aGlimmerOfHope = event AGlimmerOfHope Cards.aGlimmerOfHope

instance RunMessage AGlimmerOfHope where
  runMessage msg e@(AGlimmerOfHope attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      discards <- selectList $ inDiscardOf iid <> basic (cardIs Cards.aGlimmerOfHope)
      pushAll $ ReturnToHand iid (toTarget attrs) : [AddToHand iid discards | notNull discards]
      pure e
    _ -> AGlimmerOfHope <$> runMessage msg attrs
