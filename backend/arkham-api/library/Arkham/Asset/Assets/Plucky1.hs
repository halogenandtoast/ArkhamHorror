module Arkham.Asset.Assets.Plucky1 (plucky1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher

newtype Plucky1 = Plucky1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plucky1 :: AssetCard Plucky1
plucky1 = assetWith Plucky1 Cards.plucky1 (sanityL ?~ 1)

instance HasAbilities Plucky1 where
  getAbilities (Plucky1 x) =
    [ (cardI18n $ withI18nTooltip "plucky1.fastSpend1ResourceYouGet1WillpowerForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled x 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "plucky1.fastSpend1ResourceYouGet1IntellectForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlled x 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor Plucky1 where
  getModifiersFor (Plucky1 attrs) = modifySelf attrs [NonDirectHorrorMustBeAssignToThisFirst]

instance RunMessage Plucky1 where
  runMessage msg a@(Plucky1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    _ -> Plucky1 <$> liftRunMessage msg attrs
