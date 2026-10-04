module Arkham.Homebrew.AgainstTheWendigo.Assets.IthaquasKnowledge (ithaquasKnowledge) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards

newtype IthaquasKnowledge = IthaquasKnowledge AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ithaquasKnowledge :: AssetCard IthaquasKnowledge
ithaquasKnowledge = asset IthaquasKnowledge Cards.ithaquasKnowledge

instance HasAbilities IthaquasKnowledge where
  getAbilities (IthaquasKnowledge a) =
    [restricted a 1 ControlsThis $ FastAbility Free]

instance RunMessage IthaquasKnowledge where
  runMessage msg a@(IthaquasKnowledge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "You get +3 [intellect] until the end of your turn. Test [willpower] (3):
      -- if you fail, take 1 direct horror."
      turnModifier iid (attrs.ability 1) iid (SkillModifier #intellect 3)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directHorror iid (attrs.ability 1) 1
      pure a
    _ -> IthaquasKnowledge <$> liftRunMessage msg attrs
