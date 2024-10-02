module Arkham.Event.Events.OccultInvocation (occultInvocation, OccultInvocation (..)) where

import Arkham.Aspect hiding (aspect)
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Modifier

newtype OccultInvocation = OccultInvocation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultInvocation :: EventCard OccultInvocation
occultInvocation = event OccultInvocation Cards.occultInvocation

instance RunMessage OccultInvocation where
  runMessage msg e@(OccultInvocation attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      let n = totalDiscardCardPayments attrs.payment
      sid <- getRandom
      skillTestModifiers sid attrs iid [DamageDealt n, SkillModifier #intellect n]
      aspect iid attrs (#intellect `InsteadOf` #combat) (mkChooseFight sid iid attrs)
      pure e
    _ -> OccultInvocation <$> liftRunMessage msg attrs
