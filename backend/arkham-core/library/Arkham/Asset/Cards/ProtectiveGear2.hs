module Arkham.Asset.Cards.ProtectiveGear2 (protectiveGear2, ProtectiveGear2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Hazard))

newtype ProtectiveGear2 = ProtectiveGear2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectiveGear2 :: AssetCard ProtectiveGear2
protectiveGear2 = assetWith ProtectiveGear2 Cards.protectiveGear2 $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasAbilities ProtectiveGear2 where
  getAbilities (ProtectiveGear2 x) =
    [ controlledAbility x 1 undefined
        $ ReactionAbility
          (DrawCard #when You (basic $ #treachery <> CardWithTrait Hazard) AnyDeck)
          (DamageCost (x.ability 1) (toTarget x) 1 <> HorrorCost (x.ability 1) (toTarget x) 1)
    ]

instance RunMessage ProtectiveGear2 where
  runMessage msg a@(ProtectiveGear2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelRevelation (attrs.ability 1)
      pure a
    _ -> ProtectiveGear2 <$> lift (runMessage msg attrs)
