module Arkham.Asset.Assets.SarahVanShaw (sarahVanShaw) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype SarahVanShaw = SarahVanShaw AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sarahVanShaw :: AssetCard SarahVanShaw
sarahVanShaw = allyWith SarahVanShaw Cards.sarahVanShaw (3, 1) noSlots

resourcesPaid :: Payment -> Int
resourcesPaid (ResourcePayment n) = n
resourcesPaid (Payments ps) = sum $ map resourcesPaid ps
resourcesPaid _ = 0

instance HasAbilities SarahVanShaw where
  getAbilities (SarahVanShaw a) =
    [ withAdditionalCost (UpTo (Fixed 2) $ ResourceCost 1)
        $ fightAbility a 1 (exhaust a) ControlsThis
    ]

instance RunMessage SarahVanShaw where
  runMessage msg a@(SarahVanShaw attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (resourcesPaid -> n) -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [AddSkillValue #willpower, DamageDealt n]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ SarahVanShaw $ attrs & flippedL .~ True & visibleL .~ False
    _ -> SarahVanShaw <$> liftRunMessage msg attrs
