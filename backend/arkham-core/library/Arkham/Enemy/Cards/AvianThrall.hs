module Arkham.Enemy.Cards.AvianThrall (AvianThrall (..), avianThrall) where

import Arkham.Action qualified as Action
import Arkham.Asset.Types (Field (..))
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyFight)
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait

newtype AvianThrall = AvianThrall EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

avianThrall :: EnemyCard AvianThrall
avianThrall =
  enemyWith AvianThrall Cards.avianThrall (5, Static 4, 3) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #intellect)

instance HasModifiersFor AvianThrall where
  getModifiersFor target (AvianThrall attrs) | attrs `is` target = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mSource, mAction) of
      (Just (AssetSource aid), Just Action.Fight) -> do
        traits <- field AssetTraits aid
        let validTraits = [Ranged, Firearm, Spell]
        pure $ toModifiers attrs [Modifier.EnemyFight (-3) | any (`elem` validTraits) traits]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage AvianThrall where
  runMessage msg (AvianThrall attrs) = AvianThrall <$> runMessage msg attrs
