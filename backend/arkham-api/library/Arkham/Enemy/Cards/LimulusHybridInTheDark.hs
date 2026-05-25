module Arkham.Enemy.Cards.LimulusHybridInTheDark (limulusHybridInTheDark) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Trait
import Arkham.Window qualified as Window

newtype LimulusHybridInTheDark = LimulusHybridInTheDark EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

limulusHybridInTheDark :: EnemyCard LimulusHybridInTheDark
limulusHybridInTheDark = enemy LimulusHybridInTheDark Cards.limulusHybridInTheDark (4, Static 5, 3) (2, 1)

instance HasModifiersFor LimulusHybridInTheDark where
  getModifiersFor (LimulusHybridInTheDark a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n]

instance HasAbilities LimulusHybridInTheDark where
  getAbilities (LimulusHybridInTheDark a) =
    extend a
      [ restricted a 1 (isLight a)
          $ SilentForcedAbility
          $ oneOf [EnemyEnters #after Anywhere (be a), EnemySpawns #after Anywhere (be a)]
      , mkAbility a 2 $ forced $ EnemyAttackedSuccessfully #after You (SourceWithTrait Melee) (be a)
      ]

instance RunMessage LimulusHybridInTheDark where
  runMessage msg e@(LimulusHybridInTheDark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let lightCard = lookupCard Cards.limulusHybridInTheLight attrs.cardId
      push $ ReplaceEnemy attrs.id lightCard Swap
      checkAfter $ Window.EnemyFlipped attrs.id
      pure e
    _ -> LimulusHybridInTheDark <$> liftRunMessage msg attrs
