module Arkham.Enemy.Cards.HasturTheTatteredKing (hasturTheTatteredKing) where

import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, isFighting, isEvading)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Strategy

newtype HasturTheTatteredKing = HasturTheTatteredKing EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturTheTatteredKing :: EnemyCard HasturTheTatteredKing
hasturTheTatteredKing =
  enemyWith HasturTheTatteredKing Cards.hasturTheTatteredKing (3, PerPlayer 8, 2) (0, 4)
    $ (damageStrategyL .~ DamageFromHastur)
    . (preyL .~ Prey MostRemainingSanity)

instance HasModifiersFor HasturTheTatteredKing where
  getModifiersFor (HasturTheTatteredKing a) =
    fromMaybe mempty <$> runMaybeT do
      st <- MaybeT getSkillTest
      liftGuardM $ orM [isFighting a, isEvading a]
      liftGuardM $ fieldNone InvestigatorRemainingSanity st.investigator
      let tokens = filter ((`elem` [PlusOne, Zero, MinusOne, ElderSign]) . (.face)) st.revealedChaosTokens
      lift $ modifyEachMap a tokens \t -> [ForcedChaosTokenChange t.face [AutoFail]]

instance RunMessage HasturTheTatteredKing where
  runMessage msg (HasturTheTatteredKing attrs) = HasturTheTatteredKing <$> runMessage msg attrs
