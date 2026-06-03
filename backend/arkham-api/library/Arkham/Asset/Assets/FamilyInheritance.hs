module Arkham.Asset.Assets.FamilyInheritance (familyInheritance, FamilyInheritance (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Token qualified as Token

newtype FamilyInheritance = FamilyInheritance AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor FamilyInheritance where
  getModifiersFor (FamilyInheritance a) = controllerGets a [AsIfResourcePool a.id]

familyInheritance :: AssetCard FamilyInheritance
familyInheritance = asset FamilyInheritance Cards.familyInheritance

instance HasAbilities FamilyInheritance where
  getAbilities (FamilyInheritance a) =
    [ controlled a 1 (ResourcesOnThis (atLeast 1)) actionAbility
    , controlled_ a 2 $ forced $ TurnBegins #when You
    , controlled_ a 3 $ delayed $ forced $ TurnEnds #when You
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
    _ -> FamilyInheritance <$> runMessage msg attrs
