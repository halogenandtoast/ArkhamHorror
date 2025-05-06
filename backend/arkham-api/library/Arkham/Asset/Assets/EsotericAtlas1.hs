module Arkham.Asset.Assets.EsotericAtlas1 (esotericAtlas1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype EsotericAtlas1 = EsotericAtlas1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericAtlas1 :: AssetCard EsotericAtlas1
esotericAtlas1 = asset EsotericAtlas1 Cards.esotericAtlas1

instance HasAbilities EsotericAtlas1 where
  getAbilities (EsotericAtlas1 a) =
    [ controlled a 1 (CanMoveTo $ LocationWithDistanceFrom 2 YourLocation RevealedLocation)
        $ actionAbilityWithCost (assetUseCost a Secret 1)
    ]

instance RunMessage EsotericAtlas1 where
  runMessage msg a@(EsotericAtlas1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        getCanMoveToMatchingLocations iid attrs
          $ LocationWithDistanceFrom 2 (locationWithInvestigator iid) RevealedLocation
      chooseTargetM iid locations (moveTo attrs iid)
      pure a
    _ -> EsotericAtlas1 <$> liftRunMessage msg attrs
