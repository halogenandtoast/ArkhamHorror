module Arkham.Types.Asset.Cards.LoneWolf
  ( loneWolf
  , LoneWolf(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Timing qualified as Timing

newtype LoneWolf = LoneWolf AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loneWolf :: AssetCard LoneWolf
loneWolf = asset LoneWolf Cards.loneWolf

instance HasAbilities LoneWolf where
  getAbilities (LoneWolf x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> InvestigatorIsAlone)
        (ReactionAbility (TurnBegins Timing.When You) Free)
    ]

instance AssetRunner env => RunMessage env LoneWolf where
  runMessage msg a@(LoneWolf attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> LoneWolf <$> runMessage msg attrs
