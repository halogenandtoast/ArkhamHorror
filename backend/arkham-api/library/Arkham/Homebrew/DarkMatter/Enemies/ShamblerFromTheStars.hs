module Arkham.Homebrew.DarkMatter.Enemies.ShamblerFromTheStars (shamblerFromTheStars) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype ShamblerFromTheStars = ShamblerFromTheStars EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shamblerFromTheStars :: EnemyCard ShamblerFromTheStars
shamblerFromTheStars = enemy ShamblerFromTheStars Cards.shamblerFromTheStars

{- | "Aloof. Hunter. / Shambler from the Stars cannot be engaged except by the
ability below."
-}
instance HasModifiersFor ShamblerFromTheStars where
  getModifiersFor (ShamblerFromTheStars a) =
    modifySelf a [AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter, CannotBeEngaged]

{- | "Forced - After you trigger an [action] ability at its location: Shambler
from the Stars engages and attacks you. (Limit once per round.)"
-}
instance HasAbilities ShamblerFromTheStars where
  getAbilities (ShamblerFromTheStars a) =
    extend1 a
      $ limitedAbility (PlayerLimit PerRound 1)
      $ restricted a 1 (youExist $ at_ (locationWithEnemy a.id))
      $ forced
      $ ActivateAbility #after You AnyAbility

instance RunMessage ShamblerFromTheStars where
  runMessage msg e@(ShamblerFromTheStars attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ EngageEnemy iid attrs.id Nothing False
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> ShamblerFromTheStars <$> liftRunMessage msg attrs
