module Arkham.Homebrew.AgesUnwound.Enemies.ShamblerFromTheStars (shamblerFromTheStars) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype ShamblerFromTheStars = ShamblerFromTheStars EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Terror, Teeth and Tentacles/. Aloof and Hunter are on the card def.
shamblerFromTheStars :: EnemyCard ShamblerFromTheStars
shamblerFromTheStars = enemy ShamblerFromTheStars Cards.shamblerFromTheStars

{- | "Forced - After you trigger an [action] ability at any location: Shambler
from the Stars attacks you. (Group limit once per round.)"

"At any location" is the point: being Aloof and somewhere else is no protection,
so the window carries no location constraint.
-}
instance HasAbilities ShamblerFromTheStars where
  getAbilities (ShamblerFromTheStars a) =
    extend1 a
      $ groupLimit PerRound
      $ mkAbility a 1
      $ forced
      $ ActivateAbility #after You AbilityIsActionAbility

instance RunMessage ShamblerFromTheStars where
  runMessage msg e@(ShamblerFromTheStars attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Aloof, so it is never engaged: the attack is initiated directly.
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> ShamblerFromTheStars <$> liftRunMessage msg attrs
