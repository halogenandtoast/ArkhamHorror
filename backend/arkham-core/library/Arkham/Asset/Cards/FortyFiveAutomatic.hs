module Arkham.Asset.Cards.FortyFiveAutomatic (
  FortyFiveAutomatic (..),
  fortyFiveAutomatic,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype FortyFiveAutomatic = FortyFiveAutomatic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic :: AssetCard FortyFiveAutomatic
fortyFiveAutomatic = asset FortyFiveAutomatic Cards.fortyFiveAutomatic

instance HasAbilities FortyFiveAutomatic where
  getAbilities (FortyFiveAutomatic a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility
          (Just Action.Fight)
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifiers
                  attrs
                  (InvestigatorTarget iid)
                  [DamageDealt 1, SkillModifier SkillCombat 1]
              , ChooseFightEnemy iid source Nothing SkillCombat mempty False
              ]
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
