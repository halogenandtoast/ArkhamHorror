module Arkham.Event.Events.NameYourPrice2 (nameYourPrice2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
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
      mconcealed <-
        runMaybeT $ MaybeT (getLocationOf iid) >>= MaybeT . fieldMap LocationConcealedCards headMay
      chooseOneM iid do
        targets enemies \eid -> do
          isElite <- eid <=~> EliteEnemy
          nonAttackEnemyDamage (Just iid) attrs (if isElite then 5 else 10) eid
        for_ mconcealed \card -> do
          targeting card $ doFlip iid attrs card
      pure e
    _ -> NameYourPrice2 <$> liftRunMessage msg attrs
