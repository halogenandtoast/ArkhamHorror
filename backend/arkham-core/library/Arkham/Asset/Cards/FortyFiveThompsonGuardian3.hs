module Arkham.Asset.Cards.FortyFiveThompsonGuardian3
  ( fortyFiveThompsonGuardian3
  , FortyFiveThompsonGuardian3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType

newtype FortyFiveThompsonGuardian3 = FortyFiveThompsonGuardian3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompsonGuardian3 :: AssetCard FortyFiveThompsonGuardian3
fortyFiveThompsonGuardian3 = asset FortyFiveThompsonGuardian3 Cards.fortyFiveThompsonGuardian3

instance HasAbilities FortyFiveThompsonGuardian3 where
  getAbilities (FortyFiveThompsonGuardian3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage FortyFiveThompsonGuardian3 where
  runMessage msg a@(FortyFiveThompsonGuardian3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 1, SkillModifier SkillCombat 2]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    SpendUses target Ammo n | isTarget attrs target -> do
      for_ (assetController attrs) $ \iid ->
        push $ PlaceResources (toTarget iid) n
      FortyFiveThompsonGuardian3 <$> runMessage msg attrs
    _ -> FortyFiveThompsonGuardian3 <$> runMessage msg attrs
