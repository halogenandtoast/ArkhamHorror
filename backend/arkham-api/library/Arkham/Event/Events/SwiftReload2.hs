module Arkham.Event.Events.SwiftReload2 (swiftReload2) where

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Use
import Arkham.Matcher
import Arkham.Projection

newtype SwiftReload2 = SwiftReload2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftReload2 :: EventCard SwiftReload2
swiftReload2 = event SwiftReload2 Cards.swiftReload2

instance RunMessage SwiftReload2 where
  runMessage msg e@(SwiftReload2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      firearms <- select $ assetControlledBy iid <> #firearm <> AssetNotAtUsesX
      chooseTargetM iid firearms $ handleTarget iid attrs
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (AssetTarget aid) -> do
      startingUses <- fmap useCount . asStartingUses =<< field AssetStartingUses aid
      currentUses <- fieldMap AssetUses (findWithDefault 0 Ammo) aid
      addUses attrs aid Ammo $ startingUses - currentUses
      pure e
    _ -> SwiftReload2 <$> liftRunMessage msg attrs
