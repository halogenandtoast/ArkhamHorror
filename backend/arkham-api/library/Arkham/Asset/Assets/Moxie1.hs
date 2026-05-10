module Arkham.Asset.Assets.Moxie1 (moxie1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher

newtype Moxie1 = Moxie1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moxie1 :: AssetCard Moxie1
moxie1 = assetWith Moxie1 Cards.moxie1 (sanityL ?~ 1)

instance HasAbilities Moxie1 where
  getAbilities (Moxie1 x) =
    [ (cardI18n $ withI18nTooltip "moxie1.fastSpend1ResourceYouGet1WillpowerForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled x 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "moxie1.fastSpend1ResourceYouGet1AgilityForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled x 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance HasModifiersFor Moxie1 where
  getModifiersFor (Moxie1 a) = modifySelf a [NonDirectHorrorMustBeAssignToThisFirst]

instance RunMessage Moxie1 where
  runMessage msg a@(Moxie1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #agility 1)
      pure a
    _ -> Moxie1 <$> liftRunMessage msg attrs
