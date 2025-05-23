module Arkham.Asset.Assets.DiscOfItzamna2 (discOfItzamna2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (spawnedEnemy)
import Arkham.Matcher

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetCard DiscOfItzamna2
discOfItzamna2 = asset DiscOfItzamna2 Cards.discOfItzamna2

instance HasAbilities DiscOfItzamna2 where
  getAbilities (DiscOfItzamna2 a) =
    [ restricted a 1 ControlsThis
        $ triggered (EnemySpawns #when YourLocation NonEliteEnemy) (DiscardCost FromPlay (toTarget a))
    ]

instance RunMessage DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (spawnedEnemy -> enemy) _ -> do
      toDiscardBy iid (attrs.ability 1) enemy
      pure a
    _ -> DiscOfItzamna2 <$> liftRunMessage msg attrs
