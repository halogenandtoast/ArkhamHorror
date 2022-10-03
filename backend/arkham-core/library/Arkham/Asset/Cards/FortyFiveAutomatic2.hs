module Arkham.Asset.Cards.FortyFiveAutomatic2
  ( fortyFiveAutomatic2
  , FortyFiveAutomatic2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype FortyFiveAutomatic2 = FortyFiveAutomatic2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic2 :: AssetCard FortyFiveAutomatic2
fortyFiveAutomatic2 = asset FortyFiveAutomatic2 Cards.fortyFiveAutomatic2

instance HasAbilities FortyFiveAutomatic2 where
  getAbilities (FortyFiveAutomatic2 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage FortyFiveAutomatic2 where
  runMessage msg a@(FortyFiveAutomatic2 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [DamageDealt 1, SkillModifier SkillCombat 2, IgnoreRetaliate]
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    _ -> FortyFiveAutomatic2 <$> runMessage msg attrs
