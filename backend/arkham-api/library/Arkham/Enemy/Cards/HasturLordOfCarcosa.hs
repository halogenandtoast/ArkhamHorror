module Arkham.Enemy.Cards.HasturLordOfCarcosa (hasturLordOfCarcosa, HasturLordOfCarcosa (..)) where

import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype HasturLordOfCarcosa = HasturLordOfCarcosa EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturLordOfCarcosa :: EnemyCard HasturLordOfCarcosa
hasturLordOfCarcosa =
  enemyWith HasturLordOfCarcosa Cards.hasturLordOfCarcosa (3, PerPlayer 9, 3) (0, 2)
    $ (damageStrategyL .~ DamageFromHastur)
    . (preyL .~ Prey MostRemainingSanity)

instance HasModifiersFor HasturLordOfCarcosa where
  getModifiersFor (HasturLordOfCarcosa a) =
    fromMaybe mempty <$> runMaybeT do
      st <- MaybeT getSkillTest
      liftGuardM $ orM [isFighting a, isEvading a]
      liftGuardM $ fieldNone InvestigatorRemainingSanity st.investigator
      let tokens = filter ((`elem` [PlusOne, Zero, MinusOne, ElderSign]) . (.face)) st.revealedChaosTokens
      lift $ modifyEachMap a tokens \t -> [ForcedChaosTokenChange t.face [AutoFail]]

instance RunMessage HasturLordOfCarcosa where
  runMessage msg (HasturLordOfCarcosa attrs) = HasturLordOfCarcosa <$> runMessage msg attrs
