module Arkham.Asset.Cards.Thermos (
  thermos,
  Thermos (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Thermos = Thermos AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thermos :: AssetCard Thermos
thermos = asset Thermos Cards.thermos

instance HasAbilities Thermos where
  getAbilities (Thermos a) =
    [ withTooltip
        "Heal 1 damage from an investigator at your location (2 damage instead if he or she has 2 or more physical trauma)."
        $ controlledAbility
          a
          1
          (exists $ HealableInvestigator (toSource a) DamageType $ InvestigatorAt YourLocation)
        $ ActionAbility []
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
    , withTooltip
        "Heal 1 horror from an investigator at your location (2 horror instead if he or she has 2 or more mental trauma)."
        $ controlledAbility
          a
          2
          (exists $ HealableInvestigator (toSource a) HorrorType $ InvestigatorAt YourLocation)
        $ ActionAbility []
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
    ]

instance RunMessage Thermos where
  runMessage msg a@(Thermos attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payment -> do
      targets <- selectTargets $ HealableInvestigator (toSource attrs) DamageType $ colocatedWith iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ TargetLabel target [UseCardAbilityChoiceTarget iid (toSource attrs) 1 target windows' payment]
          | target <- targets
          ]
      pure a
    UseCardAbilityChoiceTarget _ (isSource attrs -> True) 1 (InvestigatorTarget iid') _ _ ->
      do
        trauma <- field InvestigatorPhysicalTrauma iid'
        push
          $ HealDamage
            (InvestigatorTarget iid')
            (toSource attrs)
            (if trauma >= 2 then 2 else 1)
        pure a
    UseCardAbility iid (isSource attrs -> True) 2 windows' payment -> do
      targets <- selectTargets $ HealableInvestigator (toSource attrs) HorrorType $ colocatedWith iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ TargetLabel target [UseCardAbilityChoiceTarget iid (toSource attrs) 2 target windows' payment]
          | target <- targets
          ]
      pure a
    UseCardAbilityChoiceTarget _ (isSource attrs -> True) 2 (InvestigatorTarget iid') _ _ ->
      do
        trauma <- field InvestigatorMentalTrauma iid'
        mHealHorror <-
          getHealHorrorMessage
            attrs
            (if trauma >= 2 then 2 else 1)
            iid'
        for_ mHealHorror push
        pure a
    _ -> Thermos <$> runMessage msg attrs
