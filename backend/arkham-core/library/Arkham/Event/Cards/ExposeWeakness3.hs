module Arkham.Event.Cards.ExposeWeakness3
  ( exposeWeakness3
  , exposeWeakness3Effect
  , ExposeWeakness3(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers hiding ( EnemyFight )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Target

newtype ExposeWeakness3 = ExposeWeakness3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness3 :: EventCard ExposeWeakness3
exposeWeakness3 = event ExposeWeakness3 Cards.exposeWeakness3

instance RunMessage ExposeWeakness3 where
  runMessage msg e@(ExposeWeakness3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      enemyIds <-
        selectList $ EnemyAt $ LocationWithInvestigator $ InvestigatorWithId iid
      enemyIdsWithFight <- traverse (traverseToSnd (field EnemyFight)) enemyIds
      pushAll
        [ chooseOne
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
        , DrawCards iid 1 False
        , Discard (toTarget attrs)
        ]
      pure e
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        mtarget <- getSkillTestTarget
        case mtarget of
          Just (EnemyTarget enemyId) -> push $ createCardEffect
            Cards.exposeWeakness3
            Nothing
            (toSource attrs)
            (EnemyTarget enemyId)
          _ -> error "had to have an enemy"
        pure e
    _ -> ExposeWeakness3 <$> runMessage msg attrs

newtype ExposeWeakness3Effect = ExposeWeakness3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness3Effect :: EffectArgs -> ExposeWeakness3Effect
exposeWeakness3Effect = cardEffect ExposeWeakness3Effect Cards.exposeWeakness3

instance HasModifiersFor ExposeWeakness3Effect where
  getModifiersFor target (ExposeWeakness3Effect a) = do
    pure $ toModifiers a [ AsIfEnemyFight 0 | effectTarget a == target ]

instance RunMessage ExposeWeakness3Effect where
  runMessage msg e@(ExposeWeakness3Effect attrs@EffectAttrs {..}) = case msg of
    EndPhase -> do
      push $ DisableEffect effectId
      pure e
    PassedSkillTest _ (Just Action.Fight) _ target _ _
      | effectTarget == target -> do
        push $ DisableEffect effectId
        pure e
    FailedSkillTest _ (Just Action.Fight) _ target _ _
      | effectTarget == target -> do
        push $ DisableEffect effectId
        pure e
    _ -> ExposeWeakness3Effect <$> runMessage msg attrs
