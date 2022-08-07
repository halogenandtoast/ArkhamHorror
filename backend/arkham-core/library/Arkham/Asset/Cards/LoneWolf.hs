module Arkham.Asset.Cards.LoneWolf
  ( loneWolf
  , LoneWolf(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LoneWolf = LoneWolf AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loneWolf :: AssetCard LoneWolf
loneWolf = asset LoneWolf Cards.loneWolf

instance HasAbilities LoneWolf where
  getAbilities (LoneWolf x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> InvestigatorIsAlone)
        (ReactionAbility (TurnBegins Timing.When You) Free)
    ]

instance RunMessage LoneWolf where
  runMessage msg a@(LoneWolf attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> LoneWolf <$> runMessage msg attrs
