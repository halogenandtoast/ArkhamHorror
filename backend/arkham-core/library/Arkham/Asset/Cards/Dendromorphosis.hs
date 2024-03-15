module Arkham.Asset.Cards.Dendromorphosis (dendromorphosis, Dendromorphosis (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype Dendromorphosis = Dendromorphosis AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dendromorphosis :: AssetCard Dendromorphosis
dendromorphosis =
  assetWith Dendromorphosis Cards.dendromorphosis
    $ (canLeavePlayByNormalMeansL .~ False)
    . (healthL ?~ 1)

instance HasAbilities Dendromorphosis where
  getAbilities (Dendromorphosis x) =
    [ restrictedAbility x 1 ControlsThis $ FastAbility $ DirectDamageCost (toSource x) You 1
    ]

instance RunMessage Dendromorphosis where
  runMessage msg a@(Dendromorphosis attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Msg.AssetDamage (toId a) (attrs.ability 1) 1 0
      pure a
    _ -> Dendromorphosis <$> runMessage msg attrs
