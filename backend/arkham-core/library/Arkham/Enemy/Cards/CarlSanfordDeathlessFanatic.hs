module Arkham.Enemy.Cards.CarlSanfordDeathlessFanatic (
  carlSanfordDeathlessFanatic,
  CarlSanfordDeathlessFanatic (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype CarlSanfordDeathlessFanatic = CarlSanfordDeathlessFanatic EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carlSanfordDeathlessFanatic :: EnemyCard CarlSanfordDeathlessFanatic
carlSanfordDeathlessFanatic =
  enemyWith
    CarlSanfordDeathlessFanatic
    Cards.carlSanfordDeathlessFanatic
    (4, PerPlayer 6, 4)
    (1, 3)
    (spawnAtL ?~ "Silver Twilight Lodge")

instance HasModifiersFor CarlSanfordDeathlessFanatic where
  getModifiersFor target (CarlSanfordDeathlessFanatic a) | isTarget a target = do
    clues <- getSum <$> selectAgg Sum InvestigatorClues UneliminatedInvestigator
    pure $ toModifiers a [HealthModifier $ negate $ 2 * clues]
  getModifiersFor _ _ = pure []

instance HasAbilities CarlSanfordDeathlessFanatic where
  getAbilities (CarlSanfordDeathlessFanatic a) =
    withBaseAbilities a
      $ [ forcedAbility a 1 $ RemovedBreaches Timing.After $ ActTargetMatches AnyAct
        ]

instance RunMessage CarlSanfordDeathlessFanatic where
  runMessage msg e@(CarlSanfordDeathlessFanatic attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      mLocation <- field EnemyLocation (toId attrs)
      for_ mLocation $ \location ->
        push $ PlaceBreaches (toTarget location) 1
      pure e
    _ -> CarlSanfordDeathlessFanatic <$> runMessage msg attrs
