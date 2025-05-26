module Arkham.Location.Cards.CloudForest (cloudForest) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.Window

newtype CloudForest = CloudForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloudForest :: LocationCard CloudForest
cloudForest = symbolLabel $ location CloudForest Cards.cloudForest 3 (PerPlayer 2)

instance HasAbilities CloudForest where
  getAbilities (CloudForest a) =
    extendRevealed1 a
      $ restricted a 1 (not_ $ HasSupply Torches)
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (at_ $ be a) AnySource

instance RunMessage CloudForest where
  runMessage msg l@(CloudForest attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (damagedEnemy -> enemy) _ -> do
      cancelEnemyDamage enemy
      pure l
    _ -> CloudForest <$> liftRunMessage msg attrs
