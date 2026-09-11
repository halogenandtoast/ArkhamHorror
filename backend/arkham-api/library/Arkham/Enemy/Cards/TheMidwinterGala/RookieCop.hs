module Arkham.Enemy.Cards.TheMidwinterGala.RookieCop (rookieCop) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype RookieCop = RookieCop EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rookieCop :: EnemyCard RookieCop
rookieCop = enemy RookieCop Cards.rookieCop

instance HasAbilities RookieCop where
  getAbilities (RookieCop a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttacked #after (You <> InvestigatorAt (locationWithEnemy a)) AnySource (NotEnemy $ be a)

instance RunMessage RookieCop where
  runMessage msg e@(RookieCop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> RookieCop <$> liftRunMessage msg attrs
