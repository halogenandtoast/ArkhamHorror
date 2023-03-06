module Arkham.Asset.Cards.Aquinnah3
  ( Aquinnah3(..)
  , aquinnah3
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Attack
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Enemy.Types ( Field (EnemyHealthDamage, EnemySanityDamage) )
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype Aquinnah3 = Aquinnah3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah3 :: AssetCard Aquinnah3
aquinnah3 = ally Aquinnah3 Cards.aquinnah3 (1, 4)

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasAbilities Aquinnah3 where
  getAbilities (Aquinnah3 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ ReactionAbility
            (Matcher.EnemyAttacks Timing.When You AnyEnemyAttack AnyEnemy)
        $ Costs
            [ExhaustCost (toTarget a), HorrorCost (toSource a) (toTarget a) 1]
    ]

instance RunMessage Aquinnah3 where
  runMessage msg a@(Aquinnah3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      enemyId <- withQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack details : queue' -> (queue', attackEnemy details)
        _ -> error "unhandled"
      healthDamage' <- field EnemyHealthDamage enemyId
      sanityDamage' <- field EnemySanityDamage enemyId
      enemyIds <- selectList
        $ EnemyAt (LocationWithInvestigator $ InvestigatorWithId iid)

      when (null enemyIds) (error "enemies have to be present")

      push $ chooseOne
        iid
        [ targetLabel
            eid
            [ EnemyDamage eid $ nonAttack source healthDamage'
            , InvestigatorAssignDamage
              iid
              (EnemySource enemyId)
              DamageAny
              0
              sanityDamage'
            ]
        | eid <- enemyIds
        ]
      pure a
    _ -> Aquinnah3 <$> runMessage msg attrs
