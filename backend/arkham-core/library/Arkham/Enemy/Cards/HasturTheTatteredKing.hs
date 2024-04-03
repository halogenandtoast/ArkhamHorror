module Arkham.Enemy.Cards.HasturTheTatteredKing (hasturTheTatteredKing, HasturTheTatteredKing (..)) where

import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype HasturTheTatteredKing = HasturTheTatteredKing EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturTheTatteredKing :: EnemyCard HasturTheTatteredKing
hasturTheTatteredKing =
  enemyWith HasturTheTatteredKing Cards.hasturTheTatteredKing (3, PerPlayer 8, 2) (0, 4)
    $ (damageStrategyL .~ DamageFromHastur)
    . (preyL .~ Prey MostRemainingSanity)

instance HasModifiersFor HasturTheTatteredKing where
  getModifiersFor (ChaosTokenTarget t) (HasturTheTatteredKing a) = do
    toModifiers a . toList <$> runMaybeT do
      guard $ t.face `elem` [PlusOne, Zero, MinusOne, ElderSign]
      guardM $ isTarget a <$> MaybeT getSkillTestTarget
      guardM $ (`elem` [#fight, #evade]) <$> MaybeT getSkillTestAction
      iid <- MaybeT getSkillTestInvestigator
      guardM $ lift $ fieldNone InvestigatorRemainingSanity iid
      pure $ ForcedChaosTokenChange t.face [AutoFail]
  getModifiersFor _ _ = pure []

instance RunMessage HasturTheTatteredKing where
  runMessage msg (HasturTheTatteredKing attrs) = HasturTheTatteredKing <$> runMessage msg attrs
