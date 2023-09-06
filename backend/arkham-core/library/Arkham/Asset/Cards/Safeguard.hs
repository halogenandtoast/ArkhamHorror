module Arkham.Asset.Cards.Safeguard (
  safeguard,
  Safeguard (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Movement
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Safeguard = Safeguard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

safeguard :: AssetCard Safeguard
safeguard = asset Safeguard Cards.safeguard

instance HasAbilities Safeguard where
  getAbilities (Safeguard a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (Moves Timing.After NotYou AnySource YourLocation (ConnectedFrom YourLocation))
        $ ExhaustCost
        $ toTarget a
    ]

getMovedToLocation :: [Window] -> LocationId
getMovedToLocation [] = error "invalid call"
getMovedToLocation ((windowType -> Window.Moves _ _ _ lid) : _) = lid
getMovedToLocation (_ : xs) = getMovedToLocation xs

instance RunMessage Safeguard where
  runMessage msg a@(Safeguard attrs) = case msg of
    UseCardAbility iid source 1 windows' _ | isSource attrs source -> do
      let lid = getMovedToLocation windows'
      push $ Move $ move source iid lid
      pure a
    _ -> Safeguard <$> runMessage msg attrs
