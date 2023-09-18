module Arkham.Asset.Cards.DecoratedSkull (
  decoratedSkull,
  DecoratedSkull (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (
  AssetDefeated,
  EnemyDefeated,
  InvestigatorDefeated,
 )
import Arkham.Matcher

newtype DecoratedSkull = DecoratedSkull AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoratedSkull :: AssetCard DecoratedSkull
decoratedSkull = asset DecoratedSkull Cards.decoratedSkull

instance HasAbilities DecoratedSkull where
  getAbilities (DecoratedSkull a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( OrWindowMatcher
              [ EnemyDefeated #after Anyone ByAny AnyEnemy
              , InvestigatorDefeated #after ByAny Anyone
              , AssetDefeated #after ByAny AllyAsset
              ]
          )
          Free
    , restrictedAbility a 2 ControlsThis (actionAbilityWithCost $ assetUseCost a Charge 1)
    ]

instance RunMessage DecoratedSkull where
  runMessage msg a@(DecoratedSkull attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AddUses (toId attrs) Charge 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawing <- drawCards iid (toAbilitySource attrs 2) 1
      pushAll [drawing, TakeResources iid 1 (toAbilitySource attrs 2) False]
      pure a
    _ -> DecoratedSkull <$> runMessage msg attrs
