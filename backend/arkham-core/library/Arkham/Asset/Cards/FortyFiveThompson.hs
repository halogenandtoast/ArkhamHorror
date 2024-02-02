module Arkham.Asset.Cards.FortyFiveThompson (
  fortyFiveThompson,
  FortyFiveThompson (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype FortyFiveThompson = FortyFiveThompson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

fortyFiveThompson :: AssetCard FortyFiveThompson
fortyFiveThompson = asset FortyFiveThompson Cards.fortyFiveThompson

instance HasAbilities FortyFiveThompson where
  getAbilities (FortyFiveThompson a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage FortyFiveThompson where
  runMessage msg a@(FortyFiveThompson attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [DamageDealt 1, SkillModifier SkillCombat 2]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    _ -> FortyFiveThompson <$> runMessage msg attrs
