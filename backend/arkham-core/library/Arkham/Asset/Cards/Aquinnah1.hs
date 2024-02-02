module Arkham.Asset.Cards.Aquinnah1 (
  Aquinnah1 (..),
  aquinnah1,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Enemy.Types (Field (EnemyHealthDamage, EnemySanityDamage))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Matcher qualified as Matcher
import Arkham.Projection

newtype Aquinnah1 = Aquinnah1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

aquinnah1 :: AssetCard Aquinnah1
aquinnah1 = ally Aquinnah1 Cards.aquinnah1 (1, 4)

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasAbilities Aquinnah1 where
  getAbilities (Aquinnah1 a) =
    [ controlledAbility
        a
        1
        (CanDealDamage <> EnemyCriteria (NotAttackingEnemy <> EnemyExists (EnemyAt YourLocation)))
        $ ReactionAbility
          (Matcher.EnemyAttacks #when You AnyEnemyAttack AnyEnemy)
          (exhaust a <> HorrorCost (toSource a) (toTarget a) 1)
    ]

-- TODO: Batch cancel
instance RunMessage Aquinnah1 where
  runMessage msg a@(Aquinnah1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      enemyId <- withQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack details : queue' -> (queue', attackEnemy details)
        _ -> error "unhandled"
      healthDamage' <- field EnemyHealthDamage enemyId
      sanityDamage' <- field EnemySanityDamage enemyId
      enemies <- selectList $ enemyAtLocationWith iid <> NotEnemy (EnemyWithId enemyId)

      when (null enemies) (error "other enemies had to be present")

      player <- getPlayer iid
      push
        $ chooseOne player
        $ targetLabels enemies
        $ \eid -> [nonAttackEnemyDamage source healthDamage' eid, assignHorror iid enemyId sanityDamage']
      pure a
    _ -> Aquinnah1 <$> runMessage msg attrs
