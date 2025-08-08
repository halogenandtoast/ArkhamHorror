module Arkham.Enemy.Cards.RookieCop (rookieCop) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype RookieCop = RookieCop EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rookieCop :: EnemyCard RookieCop
rookieCop = enemy RookieCop Cards.rookieCop (2, Static 2, 2) (1, 1)

instance HasModifiersFor RookieCop where
  getModifiersFor (RookieCop a) =
    modifySelf a [AddKeyword Keyword.Surge, AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]

instance HasAbilities RookieCop where
  getAbilities (RookieCop a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttacked #after (InvestigatorAt YourLocation) AnySource (NotEnemy $ be a)

instance RunMessage RookieCop where
  runMessage msg e@(RookieCop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> RookieCop <$> liftRunMessage msg attrs
