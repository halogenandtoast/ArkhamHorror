module Arkham.Asset.Assets.FamilyInheritance (familyInheritance, FamilyInheritance (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Token qualified as Token

newtype FamilyInheritance = FamilyInheritance AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

familyInheritance :: AssetCard FamilyInheritance
familyInheritance = asset FamilyInheritance Cards.familyInheritance

instance HasAbilities FamilyInheritance where
  getAbilities (FamilyInheritance a) =
    [ controlled a 1 (ResourcesOnThis (atLeast 1)) actionAbility
    , restricted a 2 ControlsThis $ forced $ TurnBegins #when You
    , controlled a 3 ControlsThis $ delayed $ forced $ TurnEnds #when You
    ]

instance RunMessage FamilyInheritance where
  runMessage msg a@(FamilyInheritance attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ TakeResources iid (assetResources attrs) (attrs.ability 1) False
      pure . FamilyInheritance $ attrs & tokensL %~ removeAllTokens Token.Resource
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ PlaceResources (attrs.ability 2) (toTarget attrs) 4
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      pure . FamilyInheritance $ attrs & tokensL %~ removeAllTokens Token.Resource
    -- EndTurn iid | Just iid == assetController attrs -> do
    --   FamilyInheritance <$> runMessage msg (attrs & tokensL %~ removeAllTokens Token.Resource)
    _ -> FamilyInheritance <$> runMessage msg attrs
