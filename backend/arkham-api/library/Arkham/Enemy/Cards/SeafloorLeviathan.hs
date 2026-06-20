module Arkham.Enemy.Cards.SeafloorLeviathan (seafloorLeviathan) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype SeafloorLeviathan = SeafloorLeviathan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seafloorLeviathan :: EnemyCard SeafloorLeviathan
seafloorLeviathan =
  enemyWith SeafloorLeviathan Cards.seafloorLeviathan
    $ spawnAtL
    ?~ "Barrier Core"

instance HasModifiersFor SeafloorLeviathan where
  getModifiersFor (SeafloorLeviathan a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n]

instance HasAbilities SeafloorLeviathan where
  getAbilities (SeafloorLeviathan a) =
    extend1 a $ restricted a 1 (thisIs a ReadyEnemy) $ forced $ PhaseBegins #when #enemy

instance RunMessage SeafloorLeviathan where
  runMessage msg e@(SeafloorLeviathan attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> push $ IncreaseFloodLevel lid
      pure e
    _ -> SeafloorLeviathan <$> liftRunMessage msg attrs
