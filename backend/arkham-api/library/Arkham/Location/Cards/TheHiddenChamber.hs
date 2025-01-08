module Arkham.Location.Cards.TheHiddenChamber (theHiddenChamber, TheHiddenChamber (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Name
import Arkham.Placement
import Arkham.Projection

newtype TheHiddenChamber = TheHiddenChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theHiddenChamber :: LocationCard TheHiddenChamber
theHiddenChamber = location TheHiddenChamber Cards.theHiddenChamber 3 (Static 0)

instance HasModifiersFor TheHiddenChamber where
  getModifiersFor (TheHiddenChamber a) = do
    selectOne (assetIs Assets.keyToTheChamber) >>= \case
      Just keyToTheChamber -> do
        placement <- field AssetPlacement keyToTheChamber
        modifySelf a [Blocked | placement /= AttachedToLocation a.id]
      _ -> modifySelf a [Blocked]

instance RunMessage TheHiddenChamber where
  runMessage msg (TheHiddenChamber attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      connectedLocation <- getJustLocation iid
      name <- field LocationName connectedLocation
      pushAll
        [ PlaceLocation (toId attrs) (toCard attrs)
        , AddDirectConnection (toId attrs) connectedLocation
        , AddDirectConnection connectedLocation (toId attrs)
        , SetLocationLabel (toId attrs) $ nameToLabel name <> "HiddenChamber"
        ]
      TheHiddenChamber <$> liftRunMessage msg attrs
    -- Revealing will cause the other location to drop it's known connections
    -- So we must queue up to add it back
    _ -> TheHiddenChamber <$> liftRunMessage msg attrs
