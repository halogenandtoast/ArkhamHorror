module Arkham.Location.Cards.TheHiddenChamber (
  theHiddenChamber,
  TheHiddenChamber (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Name
import Arkham.Placement
import Arkham.Projection

newtype TheHiddenChamber = TheHiddenChamber LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

theHiddenChamber :: LocationCard TheHiddenChamber
theHiddenChamber =
  location TheHiddenChamber Cards.theHiddenChamber 3 (Static 0)

instance HasModifiersFor TheHiddenChamber where
  getModifiersFor target (TheHiddenChamber attrs) | isTarget attrs target = do
    mKeyToTheChamber <- selectOne (assetIs Assets.keyToTheChamber)
    case mKeyToTheChamber of
      Just keyToTheChamber -> do
        placement <- field AssetPlacement keyToTheChamber
        pure $ toModifiers attrs [Blocked | placement /= AttachedToLocation (toId attrs)]
      _ -> pure $ toModifiers attrs [Blocked]
  getModifiersFor _ _ = pure []

instance RunMessage TheHiddenChamber where
  runMessage msg (TheHiddenChamber attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      connectedLocation <- getJustLocation iid
      name <- field LocationName connectedLocation
      pushAll
        [ PlaceLocation (toId attrs) (toCard attrs)
        , AddDirectConnection (toId attrs) connectedLocation
        , AddDirectConnection connectedLocation (toId attrs)
        , SetLocationLabel (toId attrs) $ nameToLabel name <> "HiddenChamber"
        ]
      TheHiddenChamber <$> runMessage msg attrs
    -- Revealing will cause the other location to drop it's known connections
    -- So we must queue up to add it back
    _ -> TheHiddenChamber <$> runMessage msg attrs
