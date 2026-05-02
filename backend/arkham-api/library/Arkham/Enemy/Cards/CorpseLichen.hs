module Arkham.Enemy.Cards.CorpseLichen (corpseLichen) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher

newtype CorpseLichen = CorpseLichen EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corpseLichen :: EnemyCard CorpseLichen
corpseLichen = enemy CorpseLichen Cards.corpseLichen (4, Static 4, 3) (1, 1)

instance HasModifiersFor CorpseLichen where
  getModifiersFor (CorpseLichen a) = do
    time <- getCampaignTime
    modifySelfWhen a (time == Day) [CannotMove]

instance HasAbilities CorpseLichen where
  getAbilities (CorpseLichen a) =
    extend1 a
      $ restricted a 1 (thisExists a EnemyWithAnyDamage)
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage CorpseLichen where
  runMessage msg e@(CorpseLichen attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> CorpseLichen <$> liftRunMessage msg attrs
