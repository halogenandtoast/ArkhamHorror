module Arkham.Enemy.Cards.MiGoHarvester (miGoHarvester) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype MiGoHarvester = MiGoHarvester EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoHarvester :: EnemyCard MiGoHarvester
miGoHarvester = enemy MiGoHarvester Cards.miGoHarvester

instance HasAbilities MiGoHarvester where
  getAbilities (MiGoHarvester a) = extend1 a $ restricted a 1 (thisIs a ReadyEnemy) $ forced $ PhaseBegins #when #enemy

instance RunMessage MiGoHarvester where
  runMessage msg e@(MiGoHarvester attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mSample <- selectOne $ assetIs Assets.meteoriteSample
      attached <- maybe (pure False) (\aid -> attrs.id <=~> EnemyWithAsset (AssetWithId aid)) mSample
      if attached
        then push $ MoveToward (toTarget attrs) (LocationWithTitle "Fungus Mound")
        else do
          push $ MoveToward (toTarget attrs) (LocationWithAsset $ assetIs Assets.meteoriteSample)
          -- Check co-location for the attach *after* the move resolves.
          doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      whenM (selectAny $ assetIs Assets.meteoriteSample <> AssetAt (locationWithEnemy attrs)) do
        selectEach (assetIs Assets.meteoriteSample) (`place` AttachedToEnemy attrs.id)
      pure e
    _ -> MiGoHarvester <$> liftRunMessage msg attrs
