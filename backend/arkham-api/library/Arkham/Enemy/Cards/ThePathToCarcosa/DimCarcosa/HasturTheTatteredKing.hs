module Arkham.Enemy.Cards.ThePathToCarcosa.DimCarcosa.HasturTheTatteredKing (hasturTheTatteredKing) where

import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Enemy.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, isEvading, isFighting)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Strategy

newtype HasturTheTatteredKing = HasturTheTatteredKing EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturTheTatteredKing :: EnemyCard HasturTheTatteredKing
hasturTheTatteredKing =
  enemyWith HasturTheTatteredKing Cards.hasturTheTatteredKing
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
