module Arkham.Asset.Cards.MauserC962 (mauserC962, MauserC962 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype MauserC962 = MauserC962 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mauserC962 :: AssetCard MauserC962
mauserC962 = asset MauserC962 Cards.mauserC962

instance HasAbilities MauserC962 where
  getAbilities (MauserC962 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ exhaust a <> assetUseCost a Ammo 1]

instance RunMessage MauserC962 where
  runMessage msg a@(MauserC962 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 2]
        , chooseFight
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      canReady <-
        andM
          [ toId a <=~> AssetWithoutModifier CannotReady
          , iid <=~> InvestigatorWithoutModifier ControlledAssetsCannotReady
          ]
      canGainResources <- can.gain.resources iid
      player <- getPlayer iid
      when (canReady || canGainResources)
        $ if n >= 4
          then
            pushAll
              $ [Ready (toTarget attrs) | canReady]
              <> [TakeResources iid 1 (toSource attrs) False | canGainResources]
          else
            push
              $ chooseOrRunOne player
              $ [Label "Ready Mauser C962" [Ready (toTarget attrs)] | canReady]
              <> [Label "Take 1 resource" [TakeResources iid 1 (toSource attrs) False] | canGainResources]
      pure a
    _ -> MauserC962 <$> runMessage msg attrs
