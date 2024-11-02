module Arkham.Enemy.Cards.SerpentsOfYigAdvanced (serpentsOfYigAdvanced, SerpentsOfYigAdvanced (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SerpentsOfYigAdvanced = SerpentsOfYigAdvanced EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

serpentsOfYigAdvanced :: EnemyCard SerpentsOfYigAdvanced
serpentsOfYigAdvanced =
  enemyWith
    SerpentsOfYigAdvanced
    Cards.serpentsOfYigAdvanced
    (3, Static 4, 3)
    (1, 1)
    (\a -> a & preyL .~ BearerOf (toId a))

instance RunMessage SerpentsOfYigAdvanced where
  runMessage msg e@(SerpentsOfYigAdvanced attrs) = runQueueT $ case msg of
    Revelation _ source | isSource attrs source -> do
      selectEach (IncludeSealed $ oneOf [#eldersign, #bless]) \chaosToken -> do
        pushAll [SealChaosToken chaosToken, SealedChaosToken chaosToken (toTarget attrs)]
      pure e
    _ -> SerpentsOfYigAdvanced <$> liftRunMessage msg attrs
