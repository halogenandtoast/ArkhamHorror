module Arkham.Asset.Assets.CombatTraining1 (combatTraining1, CombatTraining1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype CombatTraining1 = CombatTraining1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

combatTraining1 :: AssetCard CombatTraining1
combatTraining1 = assetWith CombatTraining1 Cards.combatTraining1 (sanityL ?~ 1)

instance HasAbilities CombatTraining1 where
  getAbilities (CombatTraining1 x) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ wantsSkillTest (YourSkillTest #combat)
        $ controlledAbility x 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ wantsSkillTest (YourSkillTest #agility)
        $ controlledAbility x 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor CombatTraining1 where
  getModifiersFor (CombatTraining1 a) = modifySelf a [NonDirectHorrorMustBeAssignToThisFirst]

instance RunMessage CombatTraining1 where
  runMessage msg a@(CombatTraining1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 1)
      pure a
    _ -> CombatTraining1 <$> liftRunMessage msg attrs
