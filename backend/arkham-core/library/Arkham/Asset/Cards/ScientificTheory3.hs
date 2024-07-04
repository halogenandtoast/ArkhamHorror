module Arkham.Asset.Cards.ScientificTheory3 (scientificTheory3, ScientificTheory3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype ScientificTheory3 = ScientificTheory3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scientificTheory3 :: AssetCard ScientificTheory3
scientificTheory3 = assetWith ScientificTheory3 Cards.scientificTheory3 $ (healthL ?~ 1) . (sanityL ?~ 3)

instance HasAbilities ScientificTheory3 where
  getAbilities (ScientificTheory3 x) =
    [ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance HasModifiersFor ScientificTheory3 where
  getModifiersFor target (ScientificTheory3 attrs) | attrs `is` target = do
    pure
      $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
  getModifiersFor (InvestigatorTarget iid) (ScientificTheory3 attrs) | iid `controls` attrs = do
    pure $ toModifiers attrs [SkillModifier #intellect 1, SkillModifier #combat 1]
  getModifiersFor _ _ = pure []

instance RunMessage ScientificTheory3 where
  runMessage msg a@(ScientificTheory3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifiers (attrs.ability 1) iid [SkillModifier #intellect 1, SkillModifier #combat 1]
      pure a
    _ -> ScientificTheory3 <$> liftRunMessage msg attrs
