module Arkham.Enemy.Cards.HasturTheTatteredKing (
  hasturTheTatteredKing,
  HasturTheTatteredKing (..),
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
import Arkham.Strategy

newtype HasturTheTatteredKing = HasturTheTatteredKing EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturTheTatteredKing :: EnemyCard HasturTheTatteredKing
hasturTheTatteredKing =
  enemyWith
    HasturTheTatteredKing
    Cards.hasturTheTatteredKing
    (3, PerPlayer 8, 2)
    (0, 4)
    ((damageStrategyL .~ DamageFromHastur) . (preyL .~ Prey MostRemainingSanity))

instance HasModifiersFor HasturTheTatteredKing where
  getModifiersFor (ChaosTokenTarget t) (HasturTheTatteredKing a)
    | ChaosToken.chaosTokenFace t
        `elem` [ChaosToken.PlusOne, ChaosToken.Zero, ChaosToken.MinusOne, ChaosToken.ElderSign] =
        do
          mtarget <- getSkillTestTarget
          mSkillTestSource <- getSkillTestSource
          case mSkillTestSource of
            Just (SkillTestSource iid _ _ maction) ->
              if maybe False (isTarget a) mtarget
                && (maction == Just Action.Fight || maction == Just Action.Evade)
                then do
                  noRemainingSanity <- fieldP InvestigatorRemainingSanity (== 0) iid
                  pure
                    [ toModifier
                      a
                      (ForcedChaosTokenChange (ChaosToken.chaosTokenFace t) [ChaosToken.AutoFail])
                    | noRemainingSanity
                    ]
                else pure []
            _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage HasturTheTatteredKing where
  runMessage msg (HasturTheTatteredKing attrs) =
    HasturTheTatteredKing <$> runMessage msg attrs
