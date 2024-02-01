module Arkham.Asset.Cards.LoneWolf (
  loneWolf,
  LoneWolf (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LoneWolf = LoneWolf AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 1 (toAbilitySource attrs 1) False
      pure a
    _ -> LoneWolf <$> runMessage msg attrs
