module Arkham.Event.Events.Barricade (barricade, Barricade (..)) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype Barricade = Barricade EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barricade :: EventCard Barricade
barricade = event Barricade Cards.barricade

instance HasModifiersFor Barricade where
  getModifiersFor (Barricade attrs) = case attrs.attachedTo of
    Just target -> modified_ attrs target [CannotBeEnteredBy NonEliteEnemy]
    _ -> pure mempty

instance HasAbilities Barricade where
  getAbilities (Barricade x) = case x.attachedTo of
    Just (LocationTarget lid) -> [forcedAbility x 1 $ Leaves #when You $ LocationWithId lid]
    _ -> []

instance RunMessage Barricade where
  runMessage msg e@(Barricade attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      lid <- fieldJust InvestigatorLocation iid
      push $ PlaceEvent eid (AttachedToLocation lid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure e
    _ -> Barricade <$> liftRunMessage msg attrs
