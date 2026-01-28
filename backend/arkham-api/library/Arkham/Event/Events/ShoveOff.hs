module Arkham.Event.Events.ShoveOff (shoveOff) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Matcher

newtype ShoveOff = ShoveOff EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shoveOff :: EventCard ShoveOff
shoveOff = event ShoveOff Cards.shoveOff

instance RunMessage ShoveOff where
  runMessage msg e@(ShoveOff attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy $ nonAttackEnemyDamage (Just iid) attrs 1
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      atEndOfTurn attrs iid
        $ returnToHand iid
        $ CardMatcherTarget (oneOf [inDiscardOf iid, inPlayAreaOf iid] <> basic (CardWithId (toCard attrs).id))
      pure e
    _ -> ShoveOff <$> liftRunMessage msg attrs
