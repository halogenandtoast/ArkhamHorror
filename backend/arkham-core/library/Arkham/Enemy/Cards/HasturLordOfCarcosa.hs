module Arkham.Enemy.Cards.HasturLordOfCarcosa (
  hasturLordOfCarcosa,
  HasturLordOfCarcosa (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.ChaosToken qualified as ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype HasturLordOfCarcosa = HasturLordOfCarcosa EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturLordOfCarcosa :: EnemyCard HasturLordOfCarcosa
hasturLordOfCarcosa =
  enemyWith
    HasturLordOfCarcosa
    Cards.hasturLordOfCarcosa
    (3, PerPlayer 9, 3)
    (0, 2)
    ((damageStrategyL .~ DamageFromHastur) . (preyL .~ Prey MostRemainingSanity))

instance HasModifiersFor HasturLordOfCarcosa where
  getModifiersFor (ChaosTokenTarget t) (HasturLordOfCarcosa a)
    | ChaosToken.chaosTokenFace t
        `elem` [ChaosToken.PlusOne, ChaosToken.Zero, ChaosToken.MinusOne, ChaosToken.ElderSign] =
        do
          miid <- runMaybeT $ do
            target <- MaybeT getSkillTestTarget
            action <- MaybeT getSkillTestAction
            guard $ isTarget a target
            guard $ action `elem` [Action.Fight, Action.Evade]
            MaybeT getSkillTestInvestigator

          case miid of
            Just iid -> do
              noRemainingSanity <- fieldP InvestigatorRemainingSanity (== 0) iid
              pure
                [ toModifier
                  a
                  (ForcedChaosTokenChange (ChaosToken.chaosTokenFace t) [ChaosToken.AutoFail])
                | noRemainingSanity
                ]
            Nothing -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage HasturLordOfCarcosa where
  runMessage msg (HasturLordOfCarcosa attrs) =
    HasturLordOfCarcosa <$> runMessage msg attrs
