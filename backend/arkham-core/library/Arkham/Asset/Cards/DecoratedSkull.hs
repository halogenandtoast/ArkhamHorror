module Arkham.Asset.Cards.DecoratedSkull
  ( decoratedSkull
  , DecoratedSkull(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding
  ( AssetDefeated, EnemyDefeated, InvestigatorDefeated )
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DecoratedSkull = DecoratedSkull AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoratedSkull :: AssetCard DecoratedSkull
decoratedSkull = asset DecoratedSkull Cards.decoratedSkull

instance HasAbilities DecoratedSkull where
  getAbilities (DecoratedSkull a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
      (OrWindowMatcher
        [ EnemyDefeated Timing.After Anyone AnyEnemy
        , InvestigatorDefeated Timing.After AnySource ByAny Anyone
        , AssetDefeated Timing.After ByAny AllyAsset
        ]
      )
      Free
    , restrictedAbility a 2 ControlsThis
      $ ActionAbility Nothing
      $ ActionCost 1
      <> UseCost (AssetWithId $ toId a) Charge 1
    ]

instance RunMessage DecoratedSkull where
  runMessage msg a@(DecoratedSkull attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AddUses (toId attrs) Charge 1
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      pushAll [drawing, TakeResources iid 1 False]
      pure a
    _ -> DecoratedSkull <$> runMessage msg attrs
