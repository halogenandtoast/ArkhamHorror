module Arkham.Asset.Assets.ColtVestPocket2 (coltVestPocket2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude

newtype Metadata = Metadata {abilityTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ColtVestPocket2 = ColtVestPocket2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coltVestPocket2 :: AssetCard ColtVestPocket2
coltVestPocket2 = asset (ColtVestPocket2 . (`with` Metadata False)) Cards.coltVestPocket2

instance HasAbilities ColtVestPocket2 where
  getAbilities (ColtVestPocket2 (a `With` meta)) =
    restricted a 1 ControlsThis (fightAction $ assetUseCost a Ammo 1)
      : [restricted a 2 ControlsThis $ forced $ RoundEnds #when | abilityTriggered meta]

instance RunMessage ColtVestPocket2 where
  runMessage msg a@(ColtVestPocket2 (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid (attrs.ability 1)
      enabled <- skillTestModifiers sid attrs iid [SkillModifier #combat 2, DamageDealt 1]
      pushAll [enabled, chooseFight]
      pure . ColtVestPocket2 $ attrs `with` Metadata True
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ toDiscardBy iid (attrs.ability 2) attrs
      pure a
    _ -> ColtVestPocket2 . (`with` meta) <$> runMessage msg attrs
