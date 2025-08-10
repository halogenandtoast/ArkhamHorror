module Arkham.Asset.Assets.Safeguard (safeguard) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Safeguard = Safeguard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

safeguard :: AssetCard Safeguard
safeguard = asset Safeguard Cards.safeguard

instance HasAbilities Safeguard where
  getAbilities (Safeguard a) =
    [ restricted a 1 ControlsThis
        $ triggered
          ( Moves
              #after
              (affectsOthers NotYou)
              AnySource
              YourLocation
              (CanMoveToLocation You (toSource a) $ AccessibleFrom YourLocation <> CanEnterLocation You)
          )
          (exhaust a)
    ]

getMovedToLocation :: [Window] -> LocationId
getMovedToLocation [] = error "invalid call"
getMovedToLocation ((windowType -> Window.Moves _ _ _ lid) : _) = lid
getMovedToLocation (_ : xs) = getMovedToLocation xs

instance RunMessage Safeguard where
  runMessage msg a@(Safeguard attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      moveTo (attrs.ability 1) iid $ getMovedToLocation windows'
      pure a
    _ -> Safeguard <$> liftRunMessage msg attrs
