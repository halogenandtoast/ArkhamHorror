module Arkham.Asset.Cards.CombatTraining1 (
  combatTraining1,
  CombatTraining1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype CombatTraining1 = CombatTraining1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

combatTraining1 :: AssetCard CombatTraining1
combatTraining1 =
  assetWith CombatTraining1 Cards.combatTraining1 (sanityL ?~ 1)

instance HasAbilities CombatTraining1 where
  getAbilities (CombatTraining1 x) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ restrictedAbility x 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ restrictedAbility x 2 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    ]

instance HasModifiersFor CombatTraining1 where
  getModifiersFor (AssetTarget aid) (CombatTraining1 attrs)
    | toId attrs == aid =
        pure
          $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ = pure []

instance RunMessage CombatTraining1 where
  runMessage msg a@(CombatTraining1 attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              ( skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillCombat 1)
              )
    UseCardAbility iid source 2 _ _
      | isSource attrs source ->
          a
            <$ push
              ( skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillAgility 1)
              )
    _ -> CombatTraining1 <$> runMessage msg attrs
