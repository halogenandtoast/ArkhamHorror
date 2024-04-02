module Arkham.Asset.Cards.MauserC96 (mauserC96, MauserC96 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype MauserC96 = MauserC96 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mauserC96 :: AssetCard MauserC96
mauserC96 = asset MauserC96 Cards.mauserC96

instance HasAbilities MauserC96 where
  getAbilities (MauserC96 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ exhaust a <> assetUseCost a Ammo 1]

instance RunMessage MauserC96 where
  runMessage msg a@(MauserC96 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 1], chooseFight]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      canReady <-
        andM
          [ toId a <=~> AssetWithoutModifier CannotReady
          , iid <=~> InvestigatorWithoutModifier ControlledAssetsCannotReady
          ]
      canGainResources <- can.gain.resources iid
      player <- getPlayer iid
      pushWhen (canReady || canGainResources)
        $ chooseOrRunOne player
        $ [Label "Ready Mauser C96" [Ready (toTarget attrs)] | canReady]
        <> [Label "Take 1 resource" [TakeResources iid 1 (toSource attrs) False] | canGainResources]
      pure a
    _ -> MauserC96 <$> runMessage msg attrs
