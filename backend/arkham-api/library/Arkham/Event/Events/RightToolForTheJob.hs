module Arkham.Event.Events.RightToolForTheJob (rightToolForTheJob) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Tool, Weapon))

newtype RightToolForTheJob = RightToolForTheJob EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rightToolForTheJob :: EventCard RightToolForTheJob
rightToolForTheJob = event RightToolForTheJob Cards.rightToolForTheJob

instance RunMessage RightToolForTheJob where
  runMessage msg e@(RightToolForTheJob attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search
        iid
        attrs
        iid
        [fromTopOfDeck 9]
        (basic $ #asset <> hasAnyTrait [Tool, Weapon])
        (AddFoundToHand iid 1)
      pure e
    _ -> RightToolForTheJob <$> liftRunMessage msg attrs
