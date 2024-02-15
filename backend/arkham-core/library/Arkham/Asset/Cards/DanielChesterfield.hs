module Arkham.Asset.Cards.DanielChesterfield (
  danielChesterfield,
  DanielChesterfield (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype DanielChesterfield = DanielChesterfield AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: AssetCard DanielChesterfield
danielChesterfield = ally DanielChesterfield Cards.danielChesterfield (1, 3)

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield a) =
    [ controlledAbility a 1 (exists (NotYou <> InvestigatorAt YourLocation)) $ FastAbility Free
    , restrictedAbility a 2 ControlsThis
        $ ForcedAbility
        $ AssignedHorror #after You (ExcludesTarget $ TargetIs $ toTarget a)
    , mkAbility a 3
        $ ForcedAbility
        $ AssetLeavesPlay #when
        $ AssetWithId
        $ toId a
    ]

instance RunMessage DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      otherInvestigators <- select (InvestigatorAt YourLocation <> NotYou)
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel i [TakeControlOfAsset i (toId attrs)] | i <- otherInvestigators]
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ assignDamage iid (toAbilitySource attrs 2) 1
      pure a
    UseCardAbility _ source 3 _ _ | isSource attrs source -> do
      push (RemoveFromGame $ toTarget attrs)
      pure a
    _ -> DanielChesterfield <$> runMessage msg attrs
