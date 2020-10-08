{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.YithianObserver where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Source
import ClassyPrelude

newtype YithianObserver = YithianObserver Attrs
  deriving newtype (Show, ToJSON, FromJSON)

yithianObserver :: EnemyId -> YithianObserver
yithianObserver uuid = YithianObserver $ (baseAttrs uuid "01177")
  { enemyHealthDamage = 1
  , enemySanityDamage = 1
  , enemyFight = 4
  , enemyHealth = Static 4
  , enemyEvade = 3
  , enemyPrey = FewestCards
  }

instance HasModifiersFor env investigator YithianObserver where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env YithianObserver where
  getModifiers _ (YithianObserver Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance (IsInvestigator investigator) => HasActions env investigator YithianObserver where
  getActions i window (YithianObserver attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YithianObserver where
  runMessage msg e@(YithianObserver attrs@Attrs {..}) = case msg of
    PerformEnemyAttack iid eid | eid == enemyId -> do
      cardCount' <- unCardCount <$> asks (getCount iid)
      if cardCount' == 0
        then e <$ unshiftMessage
          (InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            (enemyHealthDamage + 1)
            (enemySanityDamage + 1)
          )
        else e <$ unshiftMessages
          [ RandomDiscard iid
          , InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            enemyHealthDamage
            enemySanityDamage
          ]
    _ -> YithianObserver <$> runMessage msg attrs
