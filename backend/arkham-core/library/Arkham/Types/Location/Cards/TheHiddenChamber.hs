module Arkham.Types.Location.Cards.TheHiddenChamber
  ( theHiddenChamber
  , TheHiddenChamber(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (theHiddenChamber)
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name

newtype TheHiddenChamber = TheHiddenChamber LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHiddenChamber :: LocationId -> TheHiddenChamber
theHiddenChamber = TheHiddenChamber . baseAttrs
  Cards.theHiddenChamber
  3
  (Static 0)
  NoSymbol
  []

instance HasId (Maybe StoryAssetId) env CardCode => HasModifiersFor env TheHiddenChamber where
  getModifiersFor _ target (TheHiddenChamber attrs) | isTarget attrs target = do
    mKeyToTheChamber <- fmap unStoryAssetId <$> getId (CardCode "02215")
    pure $ toModifiers
      attrs
      (case mKeyToTheChamber of
        Just keyToTheChamber | keyToTheChamber `member` locationAssets attrs ->
          []
        _ -> [Blocked]
      )
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TheHiddenChamber where
  getActions iid window (TheHiddenChamber attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TheHiddenChamber where
  runMessage msg (TheHiddenChamber attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      connectedLocation <- getId iid
      name <- getName connectedLocation
      unshiftMessages
        [ PlaceLocation (attrs ^. defL . cardCodeL) (toId attrs)
        , AddDirectConnection (toId attrs) connectedLocation
        , AddDirectConnection connectedLocation (toId attrs)
        , SetLocationLabel (toId attrs) $ nameToLabel name <> "HiddenChamber"
        ]
      TheHiddenChamber <$> runMessage msg attrs
    -- Revealing will cause the other location to drop it's known connections
    -- So we must queue up to add it back
    RevealLocation _ lid | lid == locationId attrs -> do
      unshiftMessages $ map
        (`AddDirectConnection` toId attrs)
        (setToList $ locationConnectedLocations attrs)
      TheHiddenChamber <$> runMessage msg attrs
    _ -> TheHiddenChamber <$> runMessage msg attrs
