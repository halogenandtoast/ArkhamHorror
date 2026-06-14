module Arkham.Asset.Assets.BarrierNodeEnergyShield (barrierNode) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype BarrierNodeEnergyShield = BarrierNodeEnergyShield AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

barrierNode :: AssetCard BarrierNodeEnergyShield
barrierNode = assetWith BarrierNodeEnergyShield Cards.barrierNode (healthL ?~ 2)

instance HasModifiersFor BarrierNodeEnergyShield where
  getModifiersFor (BarrierNodeEnergyShield a) =
    -- TODO: the +2 health bonus should only apply once all of this card's
    -- glyphs are translated; for now the base health stands.
    modifySelf a [CannotBeDefeated, CannotLeavePlay]

instance HasAbilities BarrierNodeEnergyShield where
  getAbilities (BarrierNodeEnergyShield a) =
    [controlled_ a 1 $ forced $ TurnBegins #when You]

instance RunMessage BarrierNodeEnergyShield where
  runMessage msg a@(BarrierNodeEnergyShield attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure a
    _ -> BarrierNodeEnergyShield <$> liftRunMessage msg attrs
