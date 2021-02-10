module Arkham.Types.Location.Cards.TheHiddenChamber
  ( theHiddenChamber
  , TheHiddenChamber(..)
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


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

instance LocationRunner env => RunMessage env TheHiddenChamber where
  runMessage msg (TheHiddenChamber attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      connectedLocation <- getId iid
      name <- getName connectedLocation
      unshiftMessages
        [ PlaceLocation (toId attrs)
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
