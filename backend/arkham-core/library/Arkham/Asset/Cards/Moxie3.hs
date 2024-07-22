module Arkham.Asset.Cards.Moxie3 (moxie3, Moxie3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype Moxie3 = Moxie3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moxie3 :: AssetCard Moxie3
moxie3 = assetWith Moxie3 Cards.moxie3 $ (healthL ?~ 3) . (sanityL ?~ 1)

instance HasAbilities Moxie3 where
  getAbilities (Moxie3 x) =
    [ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance HasModifiersFor Moxie3 where
  getModifiersFor target (Moxie3 attrs) | attrs `is` target = do
    pure
      $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
  getModifiersFor (InvestigatorTarget iid) (Moxie3 attrs) | iid `controls` attrs = do
    pure $ toModifiers attrs [SkillModifier #willpower 1, SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance RunMessage Moxie3 where
  runMessage msg a@(Moxie3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #willpower 1, SkillModifier #agility 1]
      pure a
    _ -> Moxie3 <$> liftRunMessage msg attrs
