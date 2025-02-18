module Arkham.Asset.Assets.ColtVestPocket (coltVestPocket) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype ColtVestPocket = ColtVestPocket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coltVestPocket :: AssetCard ColtVestPocket
coltVestPocket = asset ColtVestPocket Cards.coltVestPocket

instance HasAbilities ColtVestPocket where
  getAbilities (ColtVestPocket a) =
    [ restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1
    , restricted a 2 ControlsThis $ forced $ RoundEnds #when
    ]

instance RunMessage ColtVestPocket where
  runMessage msg a@(ColtVestPocket attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid (attrs.ability 1)
      enabled <- skillTestModifiers sid attrs iid [SkillModifier #combat 1, DamageDealt 1]
      pushAll [enabled, chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (attrs.ability 2) attrs
      pure a
    _ -> ColtVestPocket <$> runMessage msg attrs
