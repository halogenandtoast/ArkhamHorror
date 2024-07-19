module Arkham.Asset.Cards.ProtectiveGear2 (protectiveGear2, ProtectiveGear2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Trait (Trait (Hazard))

newtype ProtectiveGear2 = ProtectiveGear2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectiveGear2 :: AssetCard ProtectiveGear2
protectiveGear2 = assetWith ProtectiveGear2 Cards.protectiveGear2 $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasAbilities ProtectiveGear2 where
  getAbilities (ProtectiveGear2 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (DrawCard #when You (basic $ #treachery <> CardWithTrait Hazard) AnyDeck)
          (DamageCost (x.ability 1) (toTarget x) 1 <> HorrorCost (x.ability 1) (toTarget x) 1)
    ]

instance RunMessage ProtectiveGear2 where
  runMessage msg a@(ProtectiveGear2 attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation (attrs.ability 1) card
      pure a
    _ -> ProtectiveGear2 <$> liftRunMessage msg attrs
