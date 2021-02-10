{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Treachery.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Exception
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.TreacheryId
import Arkham.Types.Trait
import Arkham.Types.Keyword
import Arkham.Types.Treachery.Runner
import qualified Data.HashMap.Strict as HashMap

data TreacheryAttrs = TreacheryAttrs
  { treacheryName :: Text
  , treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryTraits :: HashSet Trait
  , treacheryKeywords :: HashSet Keyword
  , treacheryAttachedTarget :: Maybe Target
  , treacheryOwner :: Maybe InvestigatorId
  , treacheryWeakness :: Bool
  , treacheryDoom :: Int
  , treacheryClues :: Maybe Int
  , treacheryResources :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''TreacheryAttrs

instance ToJSON TreacheryAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "treachery"
  toEncoding = genericToEncoding $ aesonOptions $ Just "treachery"

instance FromJSON TreacheryAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "treachery"

instance HasCount ResourceCount env TreacheryAttrs where
  getCount = pure . ResourceCount . fromMaybe 0 . treacheryResources

instance Entity TreacheryAttrs where
  type EntityId TreacheryAttrs = TreacheryId
  type EntityAttrs TreacheryAttrs = TreacheryAttrs
  toId = treacheryId
  toAttrs = id

instance NamedEntity TreacheryAttrs where
  toName = mkName . treacheryName

instance TargetEntity TreacheryAttrs where
  toTarget = TreacheryTarget . toId
  isTarget TreacheryAttrs { treacheryId } (TreacheryTarget tid) =
    treacheryId == tid
  isTarget _ _ = False

instance SourceEntity TreacheryAttrs where
  toSource = TreacherySource . toId
  isSource TreacheryAttrs { treacheryId } (TreacherySource tid) =
    treacheryId == tid
  isSource _ _ = False

instance IsCard TreacheryAttrs where
  getCardId = unTreacheryId . treacheryId
  getCardCode = treacheryCardCode
  getTraits = treacheryTraits
  getKeywords = treacheryKeywords

-- ownedBy :: Attrs -> InvestigatorId -> Bool
-- ownedBy Attrs { treacheryOwner } iid = treacheryOwner == Just iid

treacheryOn :: Target -> TreacheryAttrs -> Bool
treacheryOn t TreacheryAttrs { treacheryAttachedTarget } =
  t `elem` treacheryAttachedTarget

treacheryOnInvestigator :: InvestigatorId -> TreacheryAttrs -> Bool
treacheryOnInvestigator = treacheryOn . InvestigatorTarget

treacheryOnEnemy :: EnemyId -> TreacheryAttrs -> Bool
treacheryOnEnemy = treacheryOn . EnemyTarget

treacheryOnLocation :: LocationId -> TreacheryAttrs -> Bool
treacheryOnLocation = treacheryOn . LocationTarget

treacheryOnAgenda :: AgendaId -> TreacheryAttrs -> Bool
treacheryOnAgenda = treacheryOn . AgendaTarget

withTreacheryEnemy :: MonadIO m => TreacheryAttrs -> (EnemyId -> m a) -> m a
withTreacheryEnemy attrs f = case treacheryAttachedTarget attrs of
  Just (EnemyTarget eid) -> f eid
  _ -> throwIO
    (InvalidState $ treacheryName attrs <> " must be attached to an enemy")

withTreacheryLocation
  :: MonadIO m => TreacheryAttrs -> (LocationId -> m a) -> m a
withTreacheryLocation attrs f = case treacheryAttachedTarget attrs of
  Just (LocationTarget lid) -> f lid
  _ -> throwIO
    (InvalidState $ treacheryName attrs <> " must be attached to a location")

withTreacheryInvestigator
  :: MonadIO m => TreacheryAttrs -> (InvestigatorId -> m a) -> m a
withTreacheryInvestigator attrs f = case treacheryAttachedTarget attrs of
  Just (InvestigatorTarget iid) -> f iid
  _ -> throwIO
    (InvalidState
    $ treacheryName attrs
    <> " must be attached to an investigator"
    )

baseAttrs :: TreacheryId -> CardCode -> TreacheryAttrs
baseAttrs tid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote
          ("missing encounter card: " <> unpack (unCardCode cardCode))
          (HashMap.lookup cardCode allEncounterCards)
        $ unTreacheryId tid
  in
    TreacheryAttrs
      { treacheryName = ecName
      , treacheryId = tid
      , treacheryCardCode = ecCardCode
      , treacheryTraits = ecTraits
      , treacheryKeywords = ecKeywords
      , treacheryAttachedTarget = Nothing
      , treacheryOwner = Nothing
      , treacheryWeakness = False
      , treacheryDoom = 0
      , treacheryClues = Nothing
      , treacheryResources = Nothing
      }

weaknessAttrs
  :: HasCallStack
  => TreacheryId
  -> Maybe InvestigatorId
  -> CardCode
  -> TreacheryAttrs
weaknessAttrs tid iid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing weakness card: " <> show cardCode)
          (HashMap.lookup cardCode allPlayerCards)
        $ unTreacheryId tid
  in
    TreacheryAttrs
      { treacheryName = pcName
      , treacheryId = tid
      , treacheryCardCode = pcCardCode
      , treacheryTraits = pcTraits
      , treacheryKeywords = pcKeywords
      , treacheryAttachedTarget = Nothing
      , treacheryOwner = iid
      , treacheryWeakness = True
      , treacheryDoom = 0
      , treacheryClues = Nothing
      , treacheryResources = Nothing
      }

instance HasActions env TreacheryAttrs where
  getActions _ _ _ = pure []

is :: Target -> TreacheryAttrs -> Bool
is (TreacheryTarget tid) t = tid == treacheryId t
is (CardCodeTarget cardCode) t = cardCode == treacheryCardCode t
is (CardIdTarget cardId) t = cardId == unTreacheryId (treacheryId t)
is _ _ = False

instance TreacheryRunner env => RunMessage env TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    InvestigatorEliminated iid
      | InvestigatorTarget iid `elem` treacheryAttachedTarget -> a
      <$ unshiftMessage (Discard $ toTarget a)
    AttachTreachery tid target | tid == treacheryId ->
      pure $ a & attachedTargetL ?~ target
    PlaceResources target n | isTarget a target -> do
      let amount = fromMaybe 0 treacheryResources + n
      pure $ a & resourcesL ?~ amount
    PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget ->
      a <$ unshiftMessage (Discard $ toTarget a)
    Discard target | target `elem` treacheryAttachedTarget ->
      a <$ unshiftMessage (Discard $ toTarget a)
    _ -> pure a
