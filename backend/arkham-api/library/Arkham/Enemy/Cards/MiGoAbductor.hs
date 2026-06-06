module Arkham.Enemy.Cards.MiGoAbductor (miGoAbductor) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype MiGoAbductor = MiGoAbductor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoAbductor :: EnemyCard MiGoAbductor
miGoAbductor = enemy MiGoAbductor Cards.miGoAbductor (4, PerPlayer 2, 2) (1, 1)

instance HasAbilities MiGoAbductor where
  getAbilities (MiGoAbductor a) = [restricted a 1 (thisIs a ReadyEnemy) $ forced $ PhaseBegins #when #enemy]

instance RunMessage MiGoAbductor where
  runMessage msg e@(MiGoAbductor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      atChemist <- selectAny $ assetIs Assets.universityChemist <> AssetAt (locationWithEnemy attrs)
      if atChemist
        then selectEach (assetIs Assets.universityChemist) \aid -> dealAssetDamage aid (attrs.ability 1) 1
        else push $ MoveToward (toTarget attrs) (LocationWithAsset $ assetIs Assets.universityChemist)
      pure e
    _ -> MiGoAbductor <$> liftRunMessage msg attrs
