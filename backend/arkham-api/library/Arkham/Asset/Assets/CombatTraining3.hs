module Arkham.Asset.Assets.CombatTraining3 (combatTraining3, CombatTraining3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype CombatTraining3 = CombatTraining3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

combatTraining3 :: AssetCard CombatTraining3
combatTraining3 = assetWith CombatTraining3 Cards.combatTraining3 $ (healthL ?~ 3) . (sanityL ?~ 1)

instance HasAbilities CombatTraining3 where
  getAbilities (CombatTraining3 x) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#combat, #agility])
        $ controlledAbility x 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor CombatTraining3 where
  getModifiersFor (CombatTraining3 a) = do
    self <-
      modifySelf a [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
    controller <- controllerGets a [SkillModifier #combat 1, SkillModifier #agility 1]
    pure $ self <> controller

instance RunMessage CombatTraining3 where
  runMessage msg a@(CombatTraining3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 1, SkillModifier #agility 1]
      pure a
    _ -> CombatTraining3 <$> liftRunMessage msg attrs
