module Arkham.Enemy.Cards.SaturniteMonarchInAnAlienLand (saturniteMonarchInAnAlienLand) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher

newtype SaturniteMonarchInAnAlienLand = SaturniteMonarchInAnAlienLand EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saturniteMonarchInAnAlienLand :: EnemyCard SaturniteMonarchInAnAlienLand
saturniteMonarchInAnAlienLand =
  enemy SaturniteMonarchInAnAlienLand Cards.saturniteMonarchInAnAlienLand (3, PerPlayer 6, 2) (2, 2)

instance HasModifiersFor SaturniteMonarchInAnAlienLand where
  getModifiersFor (SaturniteMonarchInAnAlienLand a) = do
    n <- selectCount AnyEnemy
    modifySelfWhen a (n > 1) [EnemyFight $ min 3 (n - 1)]

instance HasAbilities SaturniteMonarchInAnAlienLand where
  getAbilities (SaturniteMonarchInAnAlienLand a) =
    extend1 a
      $ restricted
        a
        1
        (thisExists a (EnemyAt (LocationWithAsset $ assetIs Assets.heliosTelescopeGateToTheCosmos)))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage SaturniteMonarchInAnAlienLand where
  runMessage msg e@(SaturniteMonarchInAnAlienLand attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
      removeTokens (attrs.ability 1) heliosTelescope Shard 1
      pure e
    _ -> SaturniteMonarchInAnAlienLand <$> liftRunMessage msg attrs
