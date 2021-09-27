module Arkham.Types.Asset.Cards.Aquinnah3
  ( Aquinnah3(..)
  , aquinnah3
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Timing qualified as Timing

newtype Aquinnah3 = Aquinnah3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
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
          (OwnsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
        $ ReactionAbility (EnemyAttacks Timing.When You AnyEnemy)
        $ Costs
            [ExhaustCost (toTarget a), HorrorCost (toSource a) (toTarget a) 1]
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
    _ -> Aquinnah3 <$> runMessage msg attrs
