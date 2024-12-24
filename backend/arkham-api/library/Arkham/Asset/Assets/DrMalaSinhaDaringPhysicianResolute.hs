module Arkham.Asset.Assets.DrMalaSinhaDaringPhysicianResolute (
  drMalaSinhaDaringPhysicianResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DrMalaSinhaDaringPhysicianResolute = DrMalaSinhaDaringPhysicianResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMalaSinhaDaringPhysicianResolute :: AssetCard DrMalaSinhaDaringPhysicianResolute
drMalaSinhaDaringPhysicianResolute =
  allyWith DrMalaSinhaDaringPhysicianResolute Cards.drMalaSinhaDaringPhysicianResolute (4, 3) noSlots

instance HasAbilities DrMalaSinhaDaringPhysicianResolute where
  getAbilities (DrMalaSinhaDaringPhysicianResolute a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ HealableInvestigator (a.ability 1) #damage (at_ YourLocation)
            , exists $ HealableAsset (a.ability 1) #damage (at_ YourLocation <> not_ (be a))
            ]
        )
        $ FastAbility (assetUseCost a Supply 1 <> exhaust a)
    ]

instance RunMessage DrMalaSinhaDaringPhysicianResolute where
  runMessage msg a@(DrMalaSinhaDaringPhysicianResolute attrs) = runQueueT $ case msg of
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
    _ -> DrMalaSinhaDaringPhysicianResolute <$> liftRunMessage msg attrs
