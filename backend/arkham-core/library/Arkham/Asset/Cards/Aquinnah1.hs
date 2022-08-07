module Arkham.Asset.Cards.Aquinnah1
  ( Aquinnah1(..)
  , aquinnah1
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Types ( Field (EnemyHealthDamage, EnemySanityDamage) )
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype Aquinnah1 = Aquinnah1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah1 :: AssetCard Aquinnah1
aquinnah1 = ally Aquinnah1 Cards.aquinnah1 (1, 4)

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasAbilities Aquinnah1 where
  getAbilities (Aquinnah1 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> EnemyCriteria
            (NotAttackingEnemy <> EnemyExists (EnemyAt YourLocation))
          )
        $ ReactionAbility
            (Matcher.EnemyAttacks Timing.When You AnyEnemyAttack AnyEnemy)
        $ Costs
            [ExhaustCost (toTarget a), HorrorCost (toSource a) (toTarget a) 1]
    ]

instance RunMessage Aquinnah1 where
  runMessage msg a@(Aquinnah1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemyId <- withQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack _ eid _ _ : queue' -> (queue', eid)
        _ -> error "unhandled"
      healthDamage' <- field EnemyHealthDamage enemyId
      sanityDamage' <- field EnemySanityDamage enemyId
      enemyIds <-
        selectList
        $ EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)
        <> NotEnemy (EnemyWithId enemyId)

      when (null enemyIds) (error "other enemies had to be present")

      a <$ push
        (chooseOne
          iid
          [ Run
              [ EnemyDamage eid iid source NonAttackDamageEffect healthDamage'
              , InvestigatorAssignDamage
                iid
                (EnemySource enemyId)
                DamageAny
                0
                sanityDamage'
              ]
          | eid <- enemyIds
          ]
        )
    _ -> Aquinnah1 <$> runMessage msg attrs
