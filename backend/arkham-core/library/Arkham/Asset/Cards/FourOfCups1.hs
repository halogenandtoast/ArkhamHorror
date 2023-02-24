module Arkham.Asset.Cards.FourOfCups1
  ( fourOfCups1
  , FourOfCups1(..)
  )
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (defaultWindows)

newtype FourOfCups1 = FourOfCups1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fourOfCups1 :: AssetCard FourOfCups1
fourOfCups1 =
  asset FourOfCups1 Cards.fourOfCups1

instance HasModifiersFor FourOfCups1 where
  getModifiersFor (InvestigatorTarget iid) (FourOfCups1 a) =
    pure $
      toModifiers a [SkillModifier SkillWillpower 1 | controlledBy a iid]
  getModifiersFor _ _ = pure []

instance HasAbilities FourOfCups1 where
  getAbilities (FourOfCups1 a) =
    [restrictedAbility a 1 InYourHand $ ReactionAbility (GameBegins Timing.When) Free]

instance RunMessage FourOfCups1 where
  runMessage msg a@(FourOfCups1 attrs) = case msg of
    InHand _ (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      push (PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid))
      pure a
    _ -> FourOfCups1 <$> runMessage msg attrs
