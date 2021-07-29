module Arkham.Types.Event.Cards.ExposeWeakness1
  ( exposeWeakness1
  , ExposeWeakness1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype ExposeWeakness1 = ExposeWeakness1 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1 :: EventCard ExposeWeakness1
exposeWeakness1 = event ExposeWeakness1 Cards.exposeWeakness1

instance HasActions env ExposeWeakness1 where
  getActions iid window (ExposeWeakness1 attrs) = getActions iid window attrs

instance HasModifiersFor env ExposeWeakness1

instance
  ( HasCount FightCount env EnemyId
  , HasSet EnemyId env LocationId
  , HasId LocationId env InvestigatorId
  , HasSkillTest env
  )
  => RunMessage env ExposeWeakness1 where
  runMessage msg e@(ExposeWeakness1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      lid <- getId @LocationId iid
      enemyIds <- getSetList @EnemyId lid
      enemyIdsWithFight <- traverse
        (traverseToSnd (fmap unFightCount . getCount))
        enemyIds
      e <$ push
        (chooseOne
          iid
          [ TargetLabel
              (EnemyTarget enemyId)
              [ BeginSkillTest
                  iid
                  (toSource attrs)
                  (EnemyTarget enemyId)
                  Nothing
                  SkillIntellect
                  enemyFight
              ]
          | (enemyId, enemyFight) <- enemyIdsWithFight
          ]
        )
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        mtarget <- getSkillTestTarget
        case mtarget of
          Just (EnemyTarget enemyId) -> e <$ pushAll
            [ CreateEffect
              "02228"
              (Just $ EffectInt n)
              source
              (EnemyTarget enemyId)
            , Discard $ toTarget attrs
            ]
          _ -> error "had to have an enemy"
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> e <$ push (Discard $ toTarget attrs)
    _ -> ExposeWeakness1 <$> runMessage msg attrs
