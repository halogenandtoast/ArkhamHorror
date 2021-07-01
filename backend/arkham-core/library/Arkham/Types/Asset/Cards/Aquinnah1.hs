module Arkham.Types.Asset.Cards.Aquinnah1
  ( Aquinnah1(..)
  , aquinnah1
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Window

newtype Aquinnah1 = Aquinnah1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah1 :: AssetCard Aquinnah1
aquinnah1 = ally Aquinnah1 Cards.aquinnah1 (1, 4)

reactionAbility :: AssetAttrs -> Ability
reactionAbility attrs = mkAbility
  (toSource attrs)
  1
  (FastAbility $ Costs
    [ ExhaustCost (toTarget attrs)
    , HorrorCost (toSource attrs) (toTarget attrs) 1
    ]
  )

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasModifiersFor env Aquinnah1 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Aquinnah1 where
  getActions iid (WhenEnemyAttacks You) (Aquinnah1 a) | ownedBy a iid = do
    locationId <- getId @LocationId iid
    enemyId <- fromQueue $ \queue ->
      let PerformEnemyAttack iid' eid : _ = dropUntilAttack queue
      in if iid' == iid then eid else error "mismatch"
    enemyIds <- filterSet (/= enemyId) <$> getSet locationId
    pure
      [ ActivateCardAbilityAction iid (reactionAbility a)
      | not (null enemyIds)
      ]
  getActions i window (Aquinnah1 x) = getActions i window x

instance AssetRunner env => RunMessage env Aquinnah1 where
  runMessage msg a@(Aquinnah1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid : queue' = dropUntilAttack queue
        in (queue', eid)
      healthDamage' <- unHealthDamageCount <$> getCount enemyId
      sanityDamage' <- unSanityDamageCount <$> getCount enemyId
      locationId <- getId @LocationId iid
      enemyIds <- filter (/= enemyId) <$> getSetList locationId

      when (null enemyIds) (error "other enemies had to be present")

      a <$ unshiftMessage
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
