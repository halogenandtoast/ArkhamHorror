module Arkham.Asset.Cards.HarlanEarnstone
  ( harlanEarnstone
  , HarlanEarnstone(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.SkillType
import Arkham.Target

newtype HarlanEarnstone = HarlanEarnstone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harlanEarnstone :: AssetCard HarlanEarnstone
harlanEarnstone = asset HarlanEarnstone Cards.harlanEarnstone

instance HasAbilities HarlanEarnstone where
  getAbilities (HarlanEarnstone a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility
            (Just Action.Parley)
            (ActionCost 1 <> DiscardTopOfDeckCost 3)
    ]

instance RunMessage HarlanEarnstone where
  runMessage msg a@(HarlanEarnstone attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ BeginSkillTest
        iid
        source
        (toTarget attrs)
        (Just Action.Parley)
        SkillWillpower
        4
      pure a
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ PlaceClues (toTarget attrs) 1
        pure a
    _ -> HarlanEarnstone <$> runMessage msg attrs
