module Arkham.Asset.Assets.Straitjacket (straitjacket) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card

newtype Straitjacket = Straitjacket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

straitjacket :: AssetCard Straitjacket
straitjacket =
  assetWith
    Straitjacket
    Cards.straitjacket
    (canLeavePlayByNormalMeansL .~ False)

-- Ability is usable by investigators at the same location due to this "ruling" from MJ:
--
-- > Story-assets are kind of their own weird thing. They have a player card
-- > type but are also considered to be scenario cards. While they are controlled
-- > by a player (or in a player’s deck or hand), they function like any other
-- > asset, and are considered to be player cards. While they are not under a
-- > player’s control, they function like scenario cards. Depending on the
-- > scenario, they may have either a player card back (like Lita Chantler) or an
-- > encounter card back. Which kind of card back they have doesn’t really make a
-- > difference here; it just affects which deck they might appear in.

instance HasAbilities Straitjacket where
  getAbilities (Straitjacket a) =
    [restricted a 1 OnSameLocation $ ActionAbility [] (ActionCost 2)]

instance RunMessage Straitjacket where
  runMessage msg a@(Straitjacket attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ Discarded (toTarget attrs) (attrs.ability 1) (toCard attrs)
      pure a
    _ -> Straitjacket <$> runMessage msg attrs
