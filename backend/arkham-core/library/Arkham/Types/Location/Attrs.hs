{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Attrs where

import Arkham.Json
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

data Attrs = Attrs
  { locationName               :: Text
  , locationId                 :: LocationId
  , locationRevealClues        :: GameValue
  , locationClues              :: Int
  , locationShroud             :: Int
  , locationRevealed           :: Bool
  , locationBlocked            :: Bool
  , locationInvestigators      :: HashSet InvestigatorId
  , locationEnemies            :: HashSet EnemyId
  , locationVictory            :: Maybe Int
  , locationSymbol             :: LocationSymbol
  , locationConnectedSymbols   :: HashSet LocationSymbol
  , locationConnectedLocations :: HashSet LocationId
  , locationTraits             :: HashSet Trait
  , locationTreacheries :: HashSet TreacheryId
  , locationAssets :: HashSet AssetId
  , locationAbilities :: [Ability]
  , locationModifiers :: [Modifier]
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "location"
  toEncoding = genericToEncoding $ aesonOptions $ Just "location"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "location"


investigators :: Lens' Attrs (HashSet InvestigatorId)
investigators =
  lens locationInvestigators $ \m x -> m { locationInvestigators = x }

treacheries :: Lens' Attrs (HashSet TreacheryId)
treacheries = lens locationTreacheries $ \m x -> m { locationTreacheries = x }

assets :: Lens' Attrs (HashSet AssetId)
assets = lens locationAssets $ \m x -> m { locationAssets = x }

connectedLocations :: Lens' Attrs (HashSet LocationId)
connectedLocations =
  lens locationConnectedLocations $ \m x -> m { locationConnectedLocations = x }

blocked :: Lens' Attrs Bool
blocked = lens locationBlocked $ \m x -> m { locationBlocked = x }

modifiers :: Lens' Attrs [Modifier]
modifiers = lens locationModifiers $ \m x -> m { locationModifiers = x }

baseAttrs
  :: LocationId
  -> Text
  -> Int
  -> GameValue
  -> LocationSymbol
  -> [LocationSymbol]
  -> Attrs
baseAttrs lid name shroud' revealClues symbol' connectedSymbols' = Attrs
  { locationName = name
  , locationId = lid
  , locationRevealClues = revealClues
  , locationClues = 0
  , locationShroud = shroud'
  , locationRevealed = False
  , locationBlocked = False
  , locationInvestigators = mempty
  , locationEnemies = mempty
  , locationVictory = Nothing
  , locationSymbol = symbol'
  , locationConnectedSymbols = HashSet.fromList connectedSymbols'
  , locationConnectedLocations = mempty
  , locationTraits = mempty
  , locationTreacheries = mempty
  , locationAssets = mempty
  , locationAbilities = mempty
  , locationModifiers = mempty
  }

clues :: Lens' Attrs Int
clues = lens locationClues $ \m x -> m { locationClues = x }

revealed :: Lens' Attrs Bool
revealed = lens locationRevealed $ \m x -> m { locationRevealed = x }

enemies :: Lens' Attrs (HashSet EnemyId)
enemies = lens locationEnemies $ \m x -> m { locationEnemies = x }

shroudValueFor :: Attrs -> Int
shroudValueFor Attrs {..} = foldr
  applyModifier
  locationShroud
  locationModifiers
 where
  applyModifier (ShroudModifier m _) n = max 0 (n + m)
  applyModifier _ n = n

instance (LocationRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    Investigate iid lid skillType tokenResponses False | lid == locationId ->
      a <$ unshiftMessage
        (BeginSkillTest
          iid
          (LocationSource lid)
          (Just Action.Investigate)
          skillType
          (shroudValueFor a)
          [SuccessfulInvestigation iid lid, InvestigatorDiscoverClues iid lid 1]
          []
          []
          tokenResponses
        )
    AddModifier (LocationTarget lid) modifier | lid == locationId ->
      pure $ a & modifiers %~ (modifier :)
    RemoveAllModifiersOnTargetFrom (LocationTarget lid) source
      | lid == locationId -> pure $ a & modifiers %~ filter
        ((source /=) . sourceOfModifier)
    PlacedLocation lid | lid == locationId ->
      a <$ unshiftMessage (AddConnection lid locationSymbol)
    AttachTreacheryToLocation tid lid | lid == locationId ->
      pure $ a & treacheries %~ HashSet.insert tid
    Discard (TreacheryTarget tid) ->
      pure $ a & treacheries %~ HashSet.delete tid
    Discard (EnemyTarget eid) -> pure $ a & enemies %~ HashSet.delete eid
    AddAssetAt aid lid | lid == locationId ->
      pure $ a & assets %~ HashSet.insert aid
    AddConnection lid symbol' | symbol' `elem` locationConnectedSymbols -> do
      unshiftMessages
        [ AddConnectionBack locationId locationSymbol
        , AddedConnection locationId lid
        ]
      pure $ a & connectedLocations %~ HashSet.insert lid
    AddConnectionBack lid symbol' | symbol' `elem` locationConnectedSymbols ->
      do
        unshiftMessage (AddedConnection locationId lid)
        pure $ a & connectedLocations %~ HashSet.insert lid
    DiscoverCluesAtLocation iid lid n | lid == locationId -> do
      let discoveredClues = min n locationClues
      a <$ unshiftMessage (DiscoverClues iid lid discoveredClues)
    AfterDiscoverClues _ lid n | lid == locationId -> pure $ a & clues -~ n
    WhenEnterLocation iid lid
      | lid /= locationId && iid `elem` locationInvestigators
      -> pure $ a & investigators %~ HashSet.delete iid -- TODO: should we broadcast leaving the location
    WhenEnterLocation iid lid | lid == locationId -> do
      unless locationRevealed $ unshiftMessage (RevealLocation lid)
      pure $ a & investigators %~ HashSet.insert iid
    EnemyMove eid lid _ | lid == locationId ->
      pure $ a & enemies %~ HashSet.delete eid
    EnemyMove eid _ lid | lid == locationId ->
      pure $ a & enemies %~ HashSet.insert eid
    EnemySpawn lid eid | lid == locationId ->
      pure $ a & enemies %~ HashSet.insert eid
    RemoveEnemy eid -> pure $ a & enemies %~ HashSet.delete eid
    EnemyDefeated eid _ _ _ -> pure $ a & enemies %~ HashSet.delete eid
    TakeControlOfAsset _ aid -> pure $ a & assets %~ HashSet.delete aid
    RevealLocation lid | lid == locationId -> do
      clueCount <- fromGameValue locationRevealClues . unPlayerCount <$> asks
        (getCount ())
      pure $ a & clues .~ clueCount & revealed .~ True
    _ -> pure a
