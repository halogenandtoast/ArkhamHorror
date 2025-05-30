module Arkham.Asset.Assets.Moxie1 (moxie1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype Moxie1 = Moxie1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moxie1 :: AssetCard Moxie1
moxie1 = assetWith Moxie1 Cards.moxie1 (sanityL ?~ 1)

instance HasAbilities Moxie1 where
  getAbilities (Moxie1 x) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ wantsSkillTest (YourSkillTest #willpower)
        $ controlled x 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ wantsSkillTest (YourSkillTest #agility)
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
