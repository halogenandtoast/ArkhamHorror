module Arkham.Asset.Assets.OnTheirHeels5 (onTheirHeels5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OnTheirHeels5 = OnTheirHeels5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheirHeels5 :: AssetCard OnTheirHeels5
onTheirHeels5 = asset OnTheirHeels5 Cards.onTheirHeels5

instance HasAbilities OnTheirHeels5 where
  getAbilities (OnTheirHeels5 a) =
    [ restricted a 1 ControlsThis
        $ triggered
          ( Enters
              #after
              You
              ( LocationWithEnemy AnyEnemy
                  <> oneOf
                    [LocationWithDiscoverableCluesBy You, LocationWithEnemy (EnemyCanBeDamagedBySource (a.ability 1))]
              )
          )
          (assetUseCost a Lead 1 <> exhaust a)
    ]

instance RunMessage OnTheirHeels5 where
  runMessage msg a@(OnTheirHeels5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        enemies <- select $ at_ (be lid) <> EnemyCanBeDamagedBySource (attrs.ability 1)
        chooseOrRunOneM iid do
          whenM (canDiscoverCluesAtYourLocation NotInvestigate iid) do
            labeled "Discover a clue at your location"
              $ discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
          targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      pure a
    _ -> OnTheirHeels5 <$> liftRunMessage msg attrs
