module Arkham.Asset.Assets.UniversityChemist (universityChemist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base

newtype UniversityChemist = UniversityChemist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

universityChemist :: AssetCard UniversityChemist
universityChemist = ally UniversityChemist Cards.universityChemist (3, 1)

instance HasAbilities UniversityChemist where
  getAbilities (UniversityChemist a) =
    [ restricted a 1 (Uncontrolled <> OnSameLocation) parleyAction_ ]

instance RunMessage UniversityChemist where
  runMessage msg a@(UniversityChemist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTestEdit sid iid (attrs.ability 1) attrs [#willpower, #intellect] (Fixed 3) \st -> st {skillTestAction = Just #parley}
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      n <- perPlayer 1
      when (attrs.token #resource + 1 >= n) $ takeControlOfAsset iid attrs
      pure a
    _ -> UniversityChemist <$> liftRunMessage msg attrs
