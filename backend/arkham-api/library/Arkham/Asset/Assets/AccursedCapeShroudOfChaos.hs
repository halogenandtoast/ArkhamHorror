module Arkham.Asset.Assets.AccursedCapeShroudOfChaos (accursedCapeShroudOfChaos) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype AccursedCapeShroudOfChaos = AccursedCapeShroudOfChaos AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accursedCapeShroudOfChaos :: AssetCard AccursedCapeShroudOfChaos
accursedCapeShroudOfChaos = assetWith AccursedCapeShroudOfChaos Cards.accursedCapeShroudOfChaos (healthL ?~ 4)

instance HasModifiersFor AccursedCapeShroudOfChaos where
  getModifiersFor (AccursedCapeShroudOfChaos a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance HasAbilities AccursedCapeShroudOfChaos where
  getAbilities (AccursedCapeShroudOfChaos a) = [mkAbility a 1 $ forced $ AssetDefeated #when ByAny (be a)]

instance RunMessage AccursedCapeShroudOfChaos where
  runMessage msg a@(AccursedCapeShroudOfChaos attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure a
    _ -> AccursedCapeShroudOfChaos <$> liftRunMessage msg attrs
