module Arkham.Types.Location.Cards.TheHiddenChamber
  ( theHiddenChamber
  , TheHiddenChamber(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TheHiddenChamber = TheHiddenChamber LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHiddenChamber :: TheHiddenChamber
theHiddenChamber = TheHiddenChamber $ base { locationVictory = Just 2 }
 where
  base = baseAttrs
    "02214"
    (Name "The Hidden Chamber" (Just "Prison of the Beast"))
    EncounterSet.BloodOnTheAltar
    3
    (Static 0)
    NoSymbol
    []
    [Dunwich]

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

-- N.B. we handle this in a sort of hacky way, because the reveal should
-- be triggered from an investigator action, we check which location they
-- are currently at. It would likely to be better to tie this directly
-- to the drawing of the card from underneath.
instance LocationRunner env => RunMessage env TheHiddenChamber where
  runMessage msg (TheHiddenChamber attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      connectedLocation <- getId iid
      name <- getName connectedLocation
      unshiftMessages
        [ AddDirectConnection lid connectedLocation
        , AddDirectConnection connectedLocation lid
        , SetLocationLabel lid $ nameToLabel name <> "HiddenChamber"
        ]
      TheHiddenChamber <$> runMessage msg attrs
    _ -> TheHiddenChamber <$> runMessage msg attrs
