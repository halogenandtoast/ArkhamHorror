module Arkham.Homebrew.CircusExMortis.Enemies.SylvesterBlake (sylvesterBlake) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (insteadOfDamage)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))

newtype SylvesterBlake = SylvesterBlake EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sylvesterBlake :: EnemyCard SylvesterBlake
sylvesterBlake = enemy SylvesterBlake Cards.sylvesterBlake

instance HasAbilities SylvesterBlake where
  getAbilities (SylvesterBlake a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyWouldTakeDamageWithAmount #when AnySource (be a) (atLeast 1)

instance RunMessage SylvesterBlake where
  runMessage msg e@(SylvesterBlake attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      sealed <- select $ SealedOnEnemy (be attrs) moonToken
      case sealed of
        token : _ -> releaseMoonToken token
        [] -> insteadOfDamage attrs \_ -> pure ()
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceEnemy attrs.id (lookupCard Cards.theBlackGoat attrs.cardId) Swap
      pure e
    _ -> SylvesterBlake <$> liftRunMessage msg attrs
