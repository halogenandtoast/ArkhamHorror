module Arkham.Asset.Assets.EsotericAtlas2 (esotericAtlas2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype EsotericAtlas2 = EsotericAtlas2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericAtlas2 :: AssetCard EsotericAtlas2
esotericAtlas2 = asset EsotericAtlas2 Cards.esotericAtlas2

instance HasAbilities EsotericAtlas2 where
  getAbilities (EsotericAtlas2 a) =
    [ controlled
        a
        1
        (CanMoveTo $ oneOf [LocationWithDistanceFrom n YourLocation RevealedLocation | n <- [1 .. 3]])
        $ actionAbilityWithCost (assetUseCost a Secret 1)
    ]

instance RunMessage EsotericAtlas2 where
  runMessage msg a@(EsotericAtlas2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        getCanMoveToMatchingLocations iid (attrs.ability 1)
          $ oneOf [LocationWithDistanceFrom n (locationWithInvestigator iid) RevealedLocation | n <- [1 .. 3]]
      chooseTargetM iid locations (moveTo (attrs.ability 1) iid)
      pure a
    _ -> EsotericAtlas2 <$> liftRunMessage msg attrs
