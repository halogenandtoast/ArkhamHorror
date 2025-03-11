module Arkham.Asset.Assets.LostArcana3 (lostArcana3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype LostArcana3 = LostArcana3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostArcana3 :: AssetCard LostArcana3
lostArcana3 = asset LostArcana3 Cards.lostArcana3

instance HasModifiersFor LostArcana3 where
  getModifiersFor (LostArcana3 a) = for_ a.controller \iid ->
    modifySelectWhen
      a
      (a.use Charge > 0)
      (not_ (AssetWithId a.id) <> assetControlledBy iid <> oneOf [#spell, #ritual])
      [ProvidesUses Charge (toSource a)]

instance HasAbilities LostArcana3 where
  getAbilities (LostArcana3 x) =
    [ restricted x 1 ControlsThis
        $ triggered (DiscoveringLastClue #after You YourLocation) (exhaust x)
    ]

instance RunMessage LostArcana3 where
  runMessage msg a@(LostArcana3 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Charge 1
      pure a
    _ -> LostArcana3 <$> liftRunMessage msg attrs
