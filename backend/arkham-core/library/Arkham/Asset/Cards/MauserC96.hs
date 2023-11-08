module Arkham.Asset.Cards.MauserC96 (
  mauserC96,
  MauserC96 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype MauserC96 = MauserC96 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mauserC96 :: AssetCard MauserC96
mauserC96 = asset MauserC96 Cards.mauserC96

instance HasAbilities MauserC96 where
  getAbilities (MauserC96 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage MauserC96 where
  runMessage msg a@(MauserC96 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 1, SkillModifier SkillCombat 1]
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
      player <- getPlayer iid
      when (canReady || canGainResources)
        $ push
        $ chooseOrRunOne player
        $ [Label "Ready Mauser C96" [Ready (toTarget attrs)] | canReady]
        <> [Label "Take 1 resource" [TakeResources iid 1 (toSource attrs) False] | canGainResources]
      pure a
    _ -> MauserC96 <$> runMessage msg attrs
