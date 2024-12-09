module Arkham.Event.Events.BreachTheDoor (breachTheDoor, BreachTheDoor (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Modifiers
import Arkham.Placement
import Arkham.Token

newtype BreachTheDoor = BreachTheDoor EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breachTheDoor :: EventCard BreachTheDoor
breachTheDoor = event BreachTheDoor Cards.breachTheDoor

instance HasModifiersFor BreachTheDoor where
  getModifiersFor (BreachTheDoor e) = case e.attachedTo of
    Just target -> modified_ e target [ShroudModifier (-n)]
    _ -> pure mempty
   where
    n = e.use Lead

instance RunMessage BreachTheDoor where
  runMessage msg e@(BreachTheDoor attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #combat (Fixed 1)
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      withLocationOf iid \lid -> do
        push $ PlaceEvent attrs.id (AttachedToLocation lid)
        placeTokens attrs attrs Lead n
      pure e
    _ -> BreachTheDoor <$> liftRunMessage msg attrs
