module Arkham.Asset.Cards.ChemistrySet (chemistrySet, ChemistrySet (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigate
import Arkham.Modifier

newtype ChemistrySet = ChemistrySet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chemistrySet :: AssetCard ChemistrySet
chemistrySet = asset ChemistrySet Cards.chemistrySet

instance HasAbilities ChemistrySet where
  getAbilities (ChemistrySet a) =
    [ restrictedAbility a 1 ControlsThis $ investigateAction (exhaust a)
    ]

instance RunMessage ChemistrySet where
  runMessage msg a@(ChemistrySet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) 2 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      when (n == 0) $ gainResourcesIfCan iid (attrs.ability 1) 2
      when (n == 2) $ drawCardsIfCan iid (attrs.ability 1) 1
      when (n == 4) $ withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure a
    _ -> ChemistrySet <$> liftRunMessage msg attrs
