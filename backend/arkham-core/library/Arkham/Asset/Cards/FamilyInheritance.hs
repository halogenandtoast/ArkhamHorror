module Arkham.Asset.Cards.FamilyInheritance
  ( familyInheritance
  , FamilyInheritance(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Asset.Runner
import Arkham.Ability
import Arkham.Criteria
import Arkham.Cost
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype FamilyInheritance = FamilyInheritance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

familyInheritance :: AssetCard FamilyInheritance
familyInheritance =
  asset FamilyInheritance Cards.familyInheritance

instance HasAbilities FamilyInheritance where
  getAbilities (FamilyInheritance a) =
    [ restrictedAbility a 1 (ControlsThis <> ResourcesOnThis (AtLeast $ Static 1)) $ ActionAbility Nothing $ ActionCost 1
    , restrictedAbility a 2 ControlsThis $ ForcedAbility $ TurnBegins Timing.When You
    ]

instance RunMessage FamilyInheritance where
  runMessage msg a@(FamilyInheritance attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ TakeResources iid (assetResources attrs) (toAbilitySource attrs 1) False
      pure . FamilyInheritance $ attrs & resourcesL .~ 0
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ PlaceResources (toTarget attrs) 4
      pure a
    EndTurn iid | Just iid == assetController attrs -> do
      FamilyInheritance <$> runMessage msg (attrs & resourcesL .~ 0)
    _ -> FamilyInheritance <$> runMessage msg attrs
