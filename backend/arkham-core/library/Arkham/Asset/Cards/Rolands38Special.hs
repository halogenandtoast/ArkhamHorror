module Arkham.Asset.Cards.Rolands38Special
  ( Rolands38Special(..)
  , rolands38Special
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Rolands38Special = Rolands38Special AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38Special :: AssetCard Rolands38Special
rolands38Special = asset Rolands38Special Cards.rolands38Special

instance HasAbilities Rolands38Special where
  getAbilities (Rolands38Special x) =
    [ restrictedAbility x 1 ControlsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Ammo 1])
    ]

instance RunMessage Rolands38Special where
  runMessage msg a@(Rolands38Special attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      anyClues <-
        selectAny $ locationWithInvestigator iid <> LocationWithAnyClues
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 1, SkillModifier SkillCombat (if anyClues then 3 else 1)]
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    _ -> Rolands38Special <$> runMessage msg attrs
