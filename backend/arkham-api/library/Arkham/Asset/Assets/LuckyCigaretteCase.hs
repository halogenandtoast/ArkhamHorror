module Arkham.Asset.Assets.LuckyCigaretteCase (luckyCigaretteCase, LuckyCigaretteCase (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype LuckyCigaretteCase = LuckyCigaretteCase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyCigaretteCase :: AssetCard LuckyCigaretteCase
luckyCigaretteCase = asset LuckyCigaretteCase Cards.luckyCigaretteCase

instance HasAbilities LuckyCigaretteCase where
  getAbilities (LuckyCigaretteCase a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SkillTestResult #after You #any $ SuccessResult $ atLeast 2) (exhaust a)
    ]

instance RunMessage LuckyCigaretteCase where
  runMessage msg a@(LuckyCigaretteCase attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> LuckyCigaretteCase <$> liftRunMessage msg attrs
