module Arkham.Asset.Cards.LuckyCigaretteCase (
  luckyCigaretteCase,
  LuckyCigaretteCase (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LuckyCigaretteCase = LuckyCigaretteCase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyCigaretteCase :: AssetCard LuckyCigaretteCase
luckyCigaretteCase = asset LuckyCigaretteCase Cards.luckyCigaretteCase

instance HasAbilities LuckyCigaretteCase where
  getAbilities (LuckyCigaretteCase a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          ( SkillTestResult
              Timing.After
              You
              AnySkillTest
              (SuccessResult $ AtLeast $ Static 2)
          )
          (ExhaustCost $ toTarget a)
    ]

instance RunMessage LuckyCigaretteCase where
  runMessage msg a@(LuckyCigaretteCase attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure a
    _ -> LuckyCigaretteCase <$> runMessage msg attrs
