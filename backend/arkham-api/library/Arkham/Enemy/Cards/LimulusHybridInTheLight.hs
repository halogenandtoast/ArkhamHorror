module Arkham.Enemy.Cards.LimulusHybridInTheLight (limulusHybridInTheLight) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))

newtype LimulusHybridInTheLight = LimulusHybridInTheLight EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

limulusHybridInTheLight :: EnemyCard LimulusHybridInTheLight
limulusHybridInTheLight = enemy LimulusHybridInTheLight Cards.limulusHybridInTheLight

instance HasModifiersFor LimulusHybridInTheLight where
  getModifiersFor (LimulusHybridInTheLight a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n, CannotMakeAttacksOfOpportunity]

instance HasAbilities LimulusHybridInTheLight where
  getAbilities (LimulusHybridInTheLight a) =
    extend a
      [ restricted a 1 (isDark a)
          $ SilentForcedAbility
          $ oneOf [EnemyEnters #after Anywhere (be a), EnemySpawns #after Anywhere (be a)]
      , mkAbility a 2 $ forced $ EnemyFlipped #after (be a)
      ]

instance RunMessage LimulusHybridInTheLight where
  runMessage msg e@(LimulusHybridInTheLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      nonAttackEnemyDamage Nothing (toSource attrs) n attrs
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let darkCard = lookupCard Cards.limulusHybridInTheDark attrs.cardId
      push $ ReplaceEnemy attrs.id darkCard Swap
      pure e
    _ -> LimulusHybridInTheLight <$> liftRunMessage msg attrs
