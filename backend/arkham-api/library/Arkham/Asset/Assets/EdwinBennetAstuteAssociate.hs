module Arkham.Asset.Assets.EdwinBennetAstuteAssociate (edwinBennetAstuteAssociate) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Portal))

newtype EdwinBennetAstuteAssociate = EdwinBennetAstuteAssociate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

edwinBennetAstuteAssociate :: AssetCard EdwinBennetAstuteAssociate
edwinBennetAstuteAssociate = ally EdwinBennetAstuteAssociate Cards.edwinBennetAstuteAssociate (2, 2)

instance HasAbilities EdwinBennetAstuteAssociate where
  getAbilities (EdwinBennetAstuteAssociate a) =
    [ restricted
        a
        1
        (OnSameLocation <> DuringTurn You <> InvestigatorExists (You <> InvestigatorAt (LocationWithTrait Portal)))
        $ FastAbility (exhaust a <> ResourceCost 2)
    ]

instance RunMessage EdwinBennetAstuteAssociate where
  runMessage msg a@(EdwinBennetAstuteAssociate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tindalos <- selectOne $ locationIs Locations.tindalos
      for_ tindalos $ moveTo (attrs.ability 1) iid
      pure a
    _ -> EdwinBennetAstuteAssociate <$> liftRunMessage msg attrs
