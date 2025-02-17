module Arkham.Event.Events.ActOfDesperation (actOfDesperation) where

import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher.Patterns
import Arkham.Modifier

newtype ActOfDesperation = ActOfDesperation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

actOfDesperation :: EventCard ActOfDesperation
actOfDesperation = event ActOfDesperation Cards.actOfDesperation

instance RunMessage ActOfDesperation where
  runMessage msg e@(ActOfDesperation attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      case attrs.payment.discards of
        [(zone, discardedCard)] -> do
          let n = discardedCard.printedCost
          sid <- getRandom
          skillTestModifiers sid attrs iid $ DamageDealt 1 : [SkillModifier #combat n | n > 0]
          when (zone == FromPlay && n > 0) do
            onSucceedByEffect sid (atLeast 0) attrs sid $ gainResources iid attrs n
          chooseFightEnemy sid iid attrs
        _ -> error $ "Invalid choice: " <> show attrs.payment
      pure e
    _ -> ActOfDesperation <$> liftRunMessage msg attrs
