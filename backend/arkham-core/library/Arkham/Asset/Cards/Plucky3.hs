module Arkham.Asset.Cards.Plucky3 (plucky3, Plucky3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (sanityL)
import Arkham.Matcher
import Arkham.Modifier

newtype Plucky3 = Plucky3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plucky3 :: AssetCard Plucky3
plucky3 = assetWith Plucky3 Cards.plucky3 $ (healthL ?~ 1) . (sanityL ?~ 3)

instance HasAbilities Plucky3 where
  getAbilities (Plucky3 x) =
    [ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance HasModifiersFor Plucky3 where
  getModifiersFor target (Plucky3 attrs) | attrs `is` target = do
    pure
      $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
  getModifiersFor (InvestigatorTarget iid) (Plucky3 attrs) | iid `controls` attrs = do
    pure $ toModifiers attrs [SkillModifier #willpower 1, SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance RunMessage Plucky3 where
  runMessage msg a@(Plucky3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifiers (attrs.ability 1) iid [SkillModifier #willpower 1, SkillModifier #intellect 1]
      pure a
    _ -> Plucky3 <$> lift (runMessage msg attrs)
