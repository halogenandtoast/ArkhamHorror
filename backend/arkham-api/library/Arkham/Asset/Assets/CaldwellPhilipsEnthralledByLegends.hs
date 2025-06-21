module Arkham.Asset.Assets.CaldwellPhilipsEnthralledByLegends (caldwellPhilipsEnthralledByLegends) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Helpers.Window (discoveredClues)
import Arkham.Matcher

newtype CaldwellPhilipsEnthralledByLegends = CaldwellPhilipsEnthralledByLegends AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caldwellPhilipsEnthralledByLegends :: AssetCard CaldwellPhilipsEnthralledByLegends
caldwellPhilipsEnthralledByLegends =
  allyWith CaldwellPhilipsEnthralledByLegends Cards.caldwellPhilipsEnthralledByLegends (2, 2) noSlots

instance HasModifiersFor CaldwellPhilipsEnthralledByLegends where
  getModifiersFor (CaldwellPhilipsEnthralledByLegends a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities CaldwellPhilipsEnthralledByLegends where
  getAbilities (CaldwellPhilipsEnthralledByLegends a) =
    [ reaction
        a
        1
        (ControlsThis <> youExist can.draw.cards)
        (exhaust a)
        (DiscoverClues #after You Anywhere (atLeast 1))
    , mkAbility a 2 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage CaldwellPhilipsEnthralledByLegends where
  runMessage msg a@(CaldwellPhilipsEnthralledByLegends attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      drawCards iid (attrs.ability 1) (min 3 n)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure a
    _ -> CaldwellPhilipsEnthralledByLegends <$> liftRunMessage msg attrs
