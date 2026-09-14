module Arkham.Homebrew.DarkMatter.Enemies.YithianGuard (yithianGuard) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype YithianGuard = YithianGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianGuard :: EnemyCard YithianGuard
yithianGuard = enemy YithianGuard Cards.yithianGuard

{- | "Forced - After you deal any amount of damage to Yithian Guard: Choose and
discard 1 card from your hand."
-}
instance HasAbilities YithianGuard where
  getAbilities (YithianGuard a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDealtDamage #after AnyDamageEffect (be a) (SourceUsedBy You)

instance RunMessage YithianGuard where
  runMessage msg e@(YithianGuard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure e
    _ -> YithianGuard <$> liftRunMessage msg attrs
