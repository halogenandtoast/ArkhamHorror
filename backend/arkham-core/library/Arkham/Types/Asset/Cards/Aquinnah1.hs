module Arkham.Types.Asset.Cards.Aquinnah1
  ( Aquinnah1(..)
  , aquinnah1
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.Source
import qualified Arkham.Types.Timing as Timing

newtype Aquinnah1 = Aquinnah1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah1 :: AssetCard Aquinnah1
aquinnah1 = ally Aquinnah1 Cards.aquinnah1 (1, 4)

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasModifiersFor env Aquinnah1

instance HasActions Aquinnah1 where
  getActions (Aquinnah1 x) =
    [ restrictedAbility
          x
          1
          (OwnsThis
          <> EnemyExists (EnemyAt YourLocation <> EnemyNotAttackingYou)
          )
        $ ReactionAbility (EnemyAttacks Timing.When You AnyEnemy)
        $ Costs [ExhaustThis, HorrorCost (toSource x) (toTarget x) 1]
    ]

instance AssetRunner env => RunMessage env Aquinnah1 where
  runMessage msg a@(Aquinnah1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid _ : queue' = dropUntilAttack queue
        in (queue', eid)
      healthDamage' <- unHealthDamageCount <$> getCount enemyId
      sanityDamage' <- unSanityDamageCount <$> getCount enemyId
      locationId <- getId @LocationId iid
      enemyIds <- filter (/= enemyId) <$> getSetList locationId

      when (null enemyIds) (error "other enemies had to be present")

      a <$ push
        (chooseOne
          iid
          [ Run
              [ EnemyDamage eid iid source healthDamage'
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
