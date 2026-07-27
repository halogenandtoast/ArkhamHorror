module Arkham.Asset.Assets.BarrierNodeEnergyShield (barrierNode) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype BarrierNodeEnergyShield = BarrierNodeEnergyShield AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierNode :: AssetCard BarrierNodeEnergyShield
barrierNode = assetWith BarrierNodeEnergyShield Cards.barrierNode (healthL ?~ 1)

instance HasModifiersFor BarrierNodeEnergyShield where
  getModifiersFor (BarrierNodeEnergyShield a) = do
    bonus <- getGlyphsAllKnown "QXGKS"
    artifactModifiers a
    modifySelf a $ CannotBeDefeated : [HealthModifier 2 | bonus]

instance HasAbilities BarrierNodeEnergyShield where
  getAbilities (BarrierNodeEnergyShield a) =
    [ controlled a 1 (thisExists a AssetWithDamage) $ freeReaction $ TurnBegins #when You
    , artifactAbility a 2
    ]

instance RunMessage BarrierNodeEnergyShield where
  runMessage msg a@(BarrierNodeEnergyShield attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      handOffArtifact iid attrs
      pure a
    _ -> BarrierNodeEnergyShield <$> liftRunMessage msg attrs
