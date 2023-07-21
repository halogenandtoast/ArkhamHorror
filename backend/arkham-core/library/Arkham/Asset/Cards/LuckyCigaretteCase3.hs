module Arkham.Asset.Cards.LuckyCigaretteCase3 (
  luckyCigaretteCase3,
  LuckyCigaretteCase3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LuckyCigaretteCase3 = LuckyCigaretteCase3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyCigaretteCase3 :: AssetCard LuckyCigaretteCase3
luckyCigaretteCase3 = asset LuckyCigaretteCase3 Cards.luckyCigaretteCase3

instance HasAbilities LuckyCigaretteCase3 where
  getAbilities (LuckyCigaretteCase3 a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          ( SkillTestResult
              Timing.After
              You
              AnySkillTest
              (SuccessResult $ AtLeast $ Static 1)
          )
          (ExhaustCost $ toTarget a)
    ]

toSuccessResult :: [Window] -> Int
toSuccessResult [] = error "invalid call"
toSuccessResult (Window _ (Window.PassSkillTest _ _ _ n) : _) = n
toSuccessResult (_ : xs) = toSuccessResult xs

instance RunMessage LuckyCigaretteCase3 where
  runMessage msg a@(LuckyCigaretteCase3 attrs) = case msg of
    UseCardAbility iid source 1 (toSuccessResult -> n) _ | isSource attrs source -> do
      push $ Search iid (toSource attrs) (toTarget iid) [fromTopOfDeck n] AnyCard (DrawFound iid 1)
      pure a
    _ -> LuckyCigaretteCase3 <$> runMessage msg attrs
