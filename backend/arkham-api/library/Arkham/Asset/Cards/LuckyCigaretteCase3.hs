module Arkham.Asset.Cards.LuckyCigaretteCase3 (luckyCigaretteCase3, LuckyCigaretteCase3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LuckyCigaretteCase3 = LuckyCigaretteCase3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyCigaretteCase3 :: AssetCard LuckyCigaretteCase3
luckyCigaretteCase3 = asset LuckyCigaretteCase3 Cards.luckyCigaretteCase3

instance HasAbilities LuckyCigaretteCase3 where
  getAbilities (LuckyCigaretteCase3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SkillTestResult #after You #any (SuccessResult $ atLeast 1)) (exhaust a)
    ]

toSuccessResult :: [Window] -> Int
toSuccessResult [] = error "invalid call"
toSuccessResult ((windowType -> Window.PassSkillTest _ _ _ n) : _) = n
toSuccessResult (_ : xs) = toSuccessResult xs

instance RunMessage LuckyCigaretteCase3 where
  runMessage msg a@(LuckyCigaretteCase3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toSuccessResult -> n) _ -> do
      search iid attrs iid [fromTopOfDeck n] #any (DrawFound iid 1)
      pure a
    _ -> LuckyCigaretteCase3 <$> liftRunMessage msg attrs
