module Arkham.Asset.Assets.PatricesViolin (patricesViolin) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PatricesViolin = PatricesViolin AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patricesViolin :: AssetCard PatricesViolin
patricesViolin = asset PatricesViolin Cards.patricesViolin

instance HasAbilities PatricesViolin where
  getAbilities (PatricesViolin x) =
    [ controlled x 1 (atYourLocation $ affectsOthers $ oneOf [can.gain.resources, can.draw.cards])
        $ freeTrigger (exhaust x <> HandDiscardCost 1 #any)
    ]

instance RunMessage PatricesViolin where
  runMessage msg a@(PatricesViolin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <-
        select $ affectsOthers $ colocatedWith iid <> oneOf [can.gain.resources, can.draw.cards]
      chooseOrRunOneM iid $ targets investigators $ handleTarget iid (attrs.ability 1)
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (InvestigatorTarget iid') -> do
      let source = attrs.ability 1
      canGainResources <- can.gain.resources iid'
      canDrawCards <- can.draw.cards iid'

      chooseOneM iid' do
        when canGainResources do
          labeled "Gain resource" $ gainResources iid' source 1
        when canDrawCards do
          labeled "Draw card" $ drawCards iid' source 1

      pure a
    _ -> PatricesViolin <$> liftRunMessage msg attrs
