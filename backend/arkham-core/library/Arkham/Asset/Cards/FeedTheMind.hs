module Arkham.Asset.Cards.FeedTheMind
  ( feedTheMind
  , FeedTheMind(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype FeedTheMind = FeedTheMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind :: AssetCard FeedTheMind
feedTheMind = asset FeedTheMind Cards.feedTheMind

instance HasAbilities FeedTheMind where
  getAbilities (FeedTheMind a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage FeedTheMind where
  runMessage msg a@(FeedTheMind attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest
        iid
        (toAbilitySource attrs 1)
        (InvestigatorTarget iid)
        Nothing
        SkillIntellect
        1
      pure a
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget{} _ (min 3 -> n)
      -> do
        drawing <- drawCards iid attrs n
        push drawing
        pure a
    _ -> FeedTheMind <$> runMessage msg attrs
