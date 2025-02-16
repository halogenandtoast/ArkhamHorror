module Arkham.Asset.Assets.SignOfXelotaphSymbolOfProtection (signOfXelotaphSymbolOfProtection) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Uses
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SignOfXelotaphSymbolOfProtection = SignOfXelotaphSymbolOfProtection AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signOfXelotaphSymbolOfProtection :: AssetCard SignOfXelotaphSymbolOfProtection
signOfXelotaphSymbolOfProtection = asset SignOfXelotaphSymbolOfProtection Cards.signOfXelotaphSymbolOfProtection

instance HasAbilities SignOfXelotaphSymbolOfProtection where
  getAbilities (SignOfXelotaphSymbolOfProtection a) =
    [ restricted a 1 ControlsThis $ forced (TurnBegins #when You)
    , controlled a 2 criteria
        $ FastAbility (HandDiscardCost 1 (basic $ cardIs Cards.signOfXelotaphSymbolOfProtection))
    ]
   where
    criteria = if a.use Charge >= 3 then Never else NoRestriction

instance RunMessage SignOfXelotaphSymbolOfProtection where
  runMessage msg a@(SignOfXelotaphSymbolOfProtection attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      if attrs.use Charge > 0
        then chooseOneM iid do
          labeled "Spend 1 charge" $ spendUses (attrs.ability 1) attrs Charge 1
          labeled "Discard Sign of Xelotaph" $ toDiscardBy iid (attrs.ability 1) attrs
        else toDiscardBy iid (attrs.ability 1) attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      pure $ SignOfXelotaphSymbolOfProtection $ attrs & tokensL %~ replenish #charge 3
    _ -> SignOfXelotaphSymbolOfProtection <$> liftRunMessage msg attrs
