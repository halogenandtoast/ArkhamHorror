module Arkham.Enemy.Cards.SerpentsOfYig
  ( serpentsOfYig
  , SerpentsOfYig(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Token
import Arkham.Scenario.Attrs (Field(..))
import Arkham.ChaosBag.Base

newtype SerpentsOfYig = SerpentsOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentsOfYig :: EnemyCard SerpentsOfYig
serpentsOfYig = enemyWith
  SerpentsOfYig
  Cards.serpentsOfYig
  (2, Static 3, 2)
  (1, 1)
  (\a -> a & preyL .~ BearerOf (toId a))

instance RunMessage SerpentsOfYig where
  runMessage msg e@(SerpentsOfYig attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      tokens <- scenarioFieldMap ScenarioChaosBag chaosBagTokens
      let mElderSignToken = find ((== ElderSign) . tokenFace) tokens
      for_ mElderSignToken $ \token -> do
        pushAll [SealToken token, SealedToken token (toCard attrs)]
      pure e
    _ -> SerpentsOfYig <$> runMessage msg attrs
