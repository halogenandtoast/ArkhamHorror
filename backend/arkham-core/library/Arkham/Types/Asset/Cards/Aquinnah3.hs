module Arkham.Types.Asset.Cards.Aquinnah3
  ( Aquinnah3(..)
  , aquinnah3
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

newtype Aquinnah3 = Aquinnah3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah3 :: AssetCard Aquinnah3
aquinnah3 = ally Aquinnah3 Cards.aquinnah3 (1, 4)

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasModifiersFor env Aquinnah3

instance HasActions Aquinnah3 where
  getActions (Aquinnah3 x) =
    [ restrictedAbility x 1 (OwnsThis <> EnemyExists (EnemyAt YourLocation))
        $ ReactionAbility (EnemyAttacks Timing.When You AnyEnemy)
        $ Costs [ExhaustThis, HorrorCost (toSource x) (toTarget x) 1]
    ]

instance AssetRunner env => RunMessage env Aquinnah3 where
  runMessage msg a@(Aquinnah3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid _ : queue' = dropUntilAttack queue
        in (queue', eid)
      healthDamage' <- unHealthDamageCount <$> getCount enemyId
      sanityDamage' <- unSanityDamageCount <$> getCount enemyId
      locationId <- getId @LocationId iid
      enemyIds <- getSetList @EnemyId locationId

      when (null enemyIds) (error "enemies have to be present")

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
    _ -> Aquinnah3 <$> runMessage msg attrs
