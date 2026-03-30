module Arkham.Event.Events.InsidiousTruths (insidiousTruths) where

import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype InsidiousTruths = InsidiousTruths EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

insidiousTruths :: EventCard InsidiousTruths
insidiousTruths = event InsidiousTruths Cards.insidiousTruths

instance RunMessage InsidiousTruths where
  runMessage msg e@(InsidiousTruths attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let n = totalDiscardCardPayments attrs.payment
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #combat (n * 2), DamageDealt n]
      chooseFightEnemy sid iid attrs
      pure e
    _ -> InsidiousTruths <$> liftRunMessage msg attrs
