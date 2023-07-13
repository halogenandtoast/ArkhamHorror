module Arkham.Enemy.Cards.SerpentsOfYig (
  serpentsOfYig,
  SerpentsOfYig (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Types (Field (..))

newtype SerpentsOfYig = SerpentsOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentsOfYig :: EnemyCard SerpentsOfYig
serpentsOfYig =
  enemyWith
    SerpentsOfYig
    Cards.serpentsOfYig
    (2, Static 3, 2)
    (1, 1)
    (\a -> a & preyL .~ BearerOf (toId a))

instance RunMessage SerpentsOfYig where
  runMessage msg e@(SerpentsOfYig attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      chaosTokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
      let mElderSignToken = find ((== ElderSign) . chaosTokenFace) chaosTokens
      for_ mElderSignToken $ \chaosToken -> do
        pushAll [SealChaosToken chaosToken, SealedChaosToken chaosToken (toCard attrs)]
      pure e
    _ -> SerpentsOfYig <$> runMessage msg attrs
