module Arkham.Asset.Cards.BerettaM19184 (
  berettaM19184,
  BerettaM19184 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype BerettaM19184 = BerettaM19184 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

berettaM19184 :: AssetCard BerettaM19184
berettaM19184 = asset BerettaM19184 Cards.berettaM19184

instance HasAbilities BerettaM19184 where
  getAbilities (BerettaM19184 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Fight) $
          ActionCost 1
            <> ExhaustCost (toTarget a)
            <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage BerettaM19184 where
  runMessage msg a@(BerettaM19184 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers attrs iid [DamageDealt 1, SkillModifier SkillCombat 4]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget {} _ n
      | isSource attrs source && n >= 2 ->
          do
            if n >= 4
              then
                pushAll
                  [ Ready (toTarget attrs)
                  , skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
                  ]
              else
                push $
                  chooseOne
                    iid
                    [ Label "Ready Beretta M1918" [Ready (toTarget attrs)]
                    , Label
                        "Deal an additional +1 damage"
                        [skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)]
                    ]
            pure a
    _ -> BerettaM19184 <$> runMessage msg attrs
