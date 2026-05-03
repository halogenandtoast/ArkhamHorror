module Arkham.Treachery.Cards.CallOfTheWild (callOfTheWild) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CallOfTheWild = CallOfTheWild TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfTheWild :: TreacheryCard CallOfTheWild
callOfTheWild = treachery CallOfTheWild Cards.callOfTheWild

instance RunMessage CallOfTheWild where
  runMessage msg t@(CallOfTheWild attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyToFallback iid AnyEnemy
      if null enemies
        then assignDamage iid attrs 2
        else chooseTargetM iid enemies \e -> initiateEnemyAttack e attrs iid
      pure t
    _ -> CallOfTheWild <$> liftRunMessage msg attrs
