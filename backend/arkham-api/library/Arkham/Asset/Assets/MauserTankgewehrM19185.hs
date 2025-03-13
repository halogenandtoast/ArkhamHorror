module Arkham.Asset.Assets.MauserTankgewehrM19185 (mauserTankgewehrM19185) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype MauserTankgewehrM19185 = MauserTankgewehrM19185 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mauserTankgewehrM19185 :: AssetCard MauserTankgewehrM19185
mauserTankgewehrM19185 = asset MauserTankgewehrM19185 Cards.mauserTankgewehrM19185

instance HasAbilities MauserTankgewehrM19185 where
  getAbilities (MauserTankgewehrM19185 a) =
    [ controlled a 1 criteria1 $ fightAbility (assetUseCost a Shell 1)
    , controlled a 2 criteria2 $ actionAbilityWithCost (exhaust a)
    ]
    where
      criteria1 = if a.ready then NoRestriction else Never
      criteria2 = if a.use Shell == 0 then NoRestriction else Never

instance RunMessage MauserTankgewehrM19185 where
  runMessage msg a@(MauserTankgewehrM19185 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do 
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #combat 5, DamageDealt 3]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do 
      pure $ MauserTankgewehrM19185 $ attrs & tokensL %~ replenish Shell 1
    _ -> MauserTankgewehrM19185 <$> liftRunMessage msg attrs
