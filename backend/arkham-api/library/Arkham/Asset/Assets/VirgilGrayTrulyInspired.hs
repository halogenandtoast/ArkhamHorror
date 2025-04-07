module Arkham.Asset.Assets.VirgilGrayTrulyInspired (virgilGrayTrulyInspired) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype VirgilGrayTrulyInspired = VirgilGrayTrulyInspired AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virgilGrayTrulyInspired :: AssetCard VirgilGrayTrulyInspired
virgilGrayTrulyInspired = allyWith VirgilGrayTrulyInspired Cards.virgilGrayTrulyInspired (1, 3) (slotsL .~ mempty)

instance HasAbilities VirgilGrayTrulyInspired where
  getAbilities (VirgilGrayTrulyInspired x) =
    [ groupLimit PerWindow $ controlled x 1 criteria $ freeReaction $ IncreasedAlarmLevel #after You
    , mkAbility x 2 $ forced $ AssetLeavesPlay #when (be x)
    ]
   where
    criteria =
      oneOf
        [ youExist $ oneOf [can.gain.resources, can.draw.cards]
        , exists $ HealableAsset (x.ability 1) #horror (be x)
        ]

instance RunMessage VirgilGrayTrulyInspired where
  runMessage msg a@(VirgilGrayTrulyInspired attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      others <- select $ not_ (InvestigatorWithId iid)
      chooseOneM iid do
        whenM (can.draw.cards iid) do
          labeled "Draw 1 card" $ drawCards iid (attrs.ability 1) 1
        whenM (can.gain.resources iid) do
          labeled "Gain 1 resource" $ gainResources iid (attrs.ability 1) 1
        whenM (selectAny $ HealableAsset (attrs.ability 1) #horror (be attrs)) do
          labeled "Heal 1 horror from Virgil Gray" $ healHorror attrs (attrs.ability 1) 1
      when (notNull others) $ chooseOrRunOneM iid $ targets others (`takeControlOfAsset` attrs)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> VirgilGrayTrulyInspired <$> liftRunMessage msg attrs
