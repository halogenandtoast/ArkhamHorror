module Arkham.Event.Events.Barricade3 (barricade3) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Barricade3 = Barricade3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade3 :: EventCard Barricade3
barricade3 = event Barricade3 Cards.barricade3

instance HasModifiersFor Barricade3 where
  getModifiersFor (Barricade3 attrs) = for_ attrs.attachedTo \target -> do
    modified_ attrs target [CannotBeEnteredBy NonEliteEnemy, SpawnNonEliteAtConnectingInstead]

instance HasAbilities Barricade3 where
  getAbilities (Barricade3 x) = case x.attachedTo of
    Just (LocationTarget lid) -> [mkAbility x 1 $ forced $ Leaves #when You $ LocationWithId lid]
    _ -> []

instance RunMessage Barricade3 where
  runMessage msg e@(Barricade3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent eid (AttachedToLocation lid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> Barricade3 <$> liftRunMessage msg attrs
