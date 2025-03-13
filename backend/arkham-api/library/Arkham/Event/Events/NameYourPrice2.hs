module Arkham.Event.Events.NameYourPrice2 (nameYourPrice2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype NameYourPrice2 = NameYourPrice2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nameYourPrice2 :: EventCard NameYourPrice2
nameYourPrice2 = eventWith NameYourPrice2 Cards.nameYourPrice2 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage NameYourPrice2 where
  runMessage msg e@(NameYourPrice2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ EnemyCanBeDamagedBySource (toSource attrs) <> at_ (locationWithInvestigator iid)
      chooseTargetM iid enemies \eid -> do
        isElite <- eid <=~> EliteEnemy
        nonAttackEnemyDamage attrs (if isElite then 5 else 10) eid
      pure e
    _ -> NameYourPrice2 <$> liftRunMessage msg attrs
