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
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturLordOfCarcosa :: EnemyCard HasturLordOfCarcosa
hasturLordOfCarcosa =
  enemyWith HasturLordOfCarcosa Cards.hasturLordOfCarcosa (3, PerPlayer 9, 3) (0, 2)
    $ (damageStrategyL .~ DamageFromHastur)
    . (preyL .~ Prey MostRemainingSanity)

instance HasModifiersFor HasturLordOfCarcosa where
  getModifiersFor (ChaosTokenTarget t) (HasturLordOfCarcosa a) = do
    toModifiers a . toList <$> runMaybeT do
      guard $ t.face `elem` [PlusOne, Zero, MinusOne, ElderSign]
      guardM $ isTarget a <$> MaybeT getSkillTestTarget
      guardM $ (`elem` [#fight, #evade]) <$> MaybeT getSkillTestAction
      iid <- MaybeT getSkillTestInvestigator
      guardM $ lift $ fieldNone InvestigatorRemainingSanity iid
      pure $ ForcedChaosTokenChange t.face [AutoFail]
  getModifiersFor _ _ = pure []

instance RunMessage HasturLordOfCarcosa where
  runMessage msg (HasturLordOfCarcosa attrs) = HasturLordOfCarcosa <$> runMessage msg attrs
