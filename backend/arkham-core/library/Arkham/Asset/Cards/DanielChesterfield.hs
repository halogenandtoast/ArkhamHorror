module Arkham.Asset.Cards.DanielChesterfield
  ( danielChesterfield
  , DanielChesterfield(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DanielChesterfield = DanielChesterfield AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: AssetCard DanielChesterfield
danielChesterfield = ally DanielChesterfield Cards.danielChesterfield (1, 3)

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> InvestigatorExists (NotYou <> InvestigatorAt YourLocation))
      $ FastAbility Free
    , restrictedAbility a 1 ControlsThis $ ForcedAbility $ AssignedHorror
      Timing.After
      You
      (ExcludesTarget $ TargetIs $ toTarget a)
    , mkAbility a 1
      $ ForcedAbility
      $ AssetLeavesPlay Timing.When
      $ AssetWithId
      $ toId a
    ]

instance RunMessage DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      otherInvestigators <- selectList (InvestigatorAt YourLocation <> NotYou)
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget i)
              [TakeControlOfAsset i (toId attrs)]
          | i <- otherInvestigators
          ]
        )
    UseCardAbility iid source 2 _ _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 1 0)
    UseCardAbility _ source 3 _ _ | isSource attrs source ->
      a <$ push (RemoveFromGame $ toTarget attrs)
    _ -> DanielChesterfield <$> runMessage msg attrs
