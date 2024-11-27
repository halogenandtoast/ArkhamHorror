module Arkham.Asset.Assets.DrMalaSinhaDaringPhysician (
  drMalaSinhaDaringPhysician,
  DrMalaSinhaDaringPhysician (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DrMalaSinhaDaringPhysician = DrMalaSinhaDaringPhysician AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMalaSinhaDaringPhysician :: AssetCard DrMalaSinhaDaringPhysician
drMalaSinhaDaringPhysician = allyWith DrMalaSinhaDaringPhysician Cards.drMalaSinhaDaringPhysician (4, 2) noSlots

instance HasAbilities DrMalaSinhaDaringPhysician where
  getAbilities (DrMalaSinhaDaringPhysician a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ HealableInvestigator (a.ability 1) #damage (at_ YourLocation)
            , exists $ HealableAsset (a.ability 1) #damage (at_ YourLocation <> not_ (be a))
            ]
        )
        $ actionAbilityWithCost (assetUseCost a Supply 1 <> exhaust a)
    ]

instance RunMessage DrMalaSinhaDaringPhysician where
  runMessage msg a@(DrMalaSinhaDaringPhysician attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #damage $ colocatedWith iid
      assets <-
        select
          $ HealableAsset (toSource attrs) #damage
          $ not_ (be attrs)
          <> at_ (locationWithInvestigator iid)

      chooseOneM iid do
        targets investigators \x -> healHorror x (attrs.ability 1) 2
        targets assets \x -> healHorror x (attrs.ability 1) 2
      pure a
    _ -> DrMalaSinhaDaringPhysician <$> liftRunMessage msg attrs
