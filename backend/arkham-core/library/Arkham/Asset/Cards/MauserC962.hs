module Arkham.Asset.Cards.MauserC962 (
  mauserC962,
  MauserC962 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype MauserC962 = MauserC962 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mauserC962 :: AssetCard MauserC962
mauserC962 = asset MauserC962 Cards.mauserC962

instance HasAbilities MauserC962 where
  getAbilities (MauserC962 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Fight) $
          ActionCost 1 <> ExhaustCost (toTarget a) <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage MauserC962 where
  runMessage msg a@(MauserC962 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 1, SkillModifier SkillCombat 2]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n | n >= 2 -> do
      canReady <-
        andM
          [ toId a <=~> AssetWithoutModifier CannotReady
          , iid <=~> InvestigatorWithoutModifier ControlledAssetsCannotReady
          ]
      canGainResources <- iid <=~> InvestigatorWithoutModifier CannotGainResources
      when (canReady || canGainResources) $
        if n >= 4
          then
            pushAll $
              [Ready (toTarget attrs) | canReady]
                <> [TakeResources iid 1 (toSource attrs) False | canGainResources]
          else
            push $
              chooseOrRunOne iid $
                [Label "Ready Mauser C962" [Ready (toTarget attrs)] | canReady]
                  <> [Label "Take 1 resource" [TakeResources iid 1 (toSource attrs) False] | canGainResources]
      pure a
    _ -> MauserC962 <$> runMessage msg attrs
