{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Attrs where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Keyword
import Arkham.Types.Treachery.Runner
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

data Attrs = Attrs
  { treacheryName :: Text
  , treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryTraits :: HashSet Trait
  , treacheryKeywords :: HashSet Keyword
  , treacheryAttachedLocation :: Maybe LocationId
  , treacheryAttachedInvestigator :: Maybe InvestigatorId
  , treacheryAttachedEnemy :: Maybe EnemyId
  , treacheryOwner :: Maybe InvestigatorId
  , treacheryWeakness :: Bool
  , treacheryResolved :: Bool -- should this be discarded
  , treacheryDoom :: Int
  , treacheryClues :: Maybe Int
  , treacheryResources :: Maybe Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "treachery"
  toEncoding = genericToEncoding $ aesonOptions $ Just "treachery"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "treachery"

instance Entity Attrs where
  type EntityId Attrs = TreacheryId
  toId = treacheryId
  toTarget = TreacheryTarget . toId
  toSource = TreacherySource . toId
  isSource Attrs { treacheryId } (TreacherySource tid) = treacheryId == tid
  isSource _ _ = False
  isTarget Attrs { treacheryId } (TreacheryTarget tid) = treacheryId == tid
  isTarget _ _ = False

ownedBy :: Attrs -> InvestigatorId -> Bool
ownedBy Attrs { treacheryAttachedInvestigator } iid =
  treacheryAttachedInvestigator == Just iid

resolved :: Lens' Attrs Bool
resolved = lens treacheryResolved $ \m x -> m { treacheryResolved = x }

attachedLocation :: Lens' Attrs (Maybe LocationId)
attachedLocation =
  lens treacheryAttachedLocation $ \m x -> m { treacheryAttachedLocation = x }

attachedEnemy :: Lens' Attrs (Maybe EnemyId)
attachedEnemy =
  lens treacheryAttachedEnemy $ \m x -> m { treacheryAttachedEnemy = x }

attachedInvestigator :: Lens' Attrs (Maybe InvestigatorId)
attachedInvestigator = lens treacheryAttachedInvestigator
  $ \m x -> m { treacheryAttachedInvestigator = x }

resources :: Lens' Attrs (Maybe Int)
resources = lens treacheryResources $ \m x -> m { treacheryResources = x }

baseAttrs :: TreacheryId -> CardCode -> Attrs
baseAttrs tid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote
          ("missing encounter card: " <> unpack (unCardCode cardCode))
          (HashMap.lookup cardCode allEncounterCards)
        $ CardId (unTreacheryId tid)
  in
    Attrs
      { treacheryName = ecName
      , treacheryId = tid
      , treacheryCardCode = ecCardCode
      , treacheryTraits = ecTraits
      , treacheryKeywords = HashSet.fromList ecKeywords
      , treacheryAttachedLocation = Nothing
      , treacheryAttachedEnemy = Nothing
      , treacheryAttachedInvestigator = Nothing
      , treacheryOwner = Nothing
      , treacheryWeakness = False
      , treacheryResolved = False
      , treacheryDoom = 0
      , treacheryClues = Nothing
      , treacheryResources = Nothing
      }

weaknessAttrs :: TreacheryId -> Maybe InvestigatorId -> CardCode -> Attrs
weaknessAttrs tid iid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          "missing weakness card"
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unTreacheryId tid)
  in
    Attrs
      { treacheryName = pcName
      , treacheryId = tid
      , treacheryCardCode = pcCardCode
      , treacheryTraits = pcTraits
      , treacheryKeywords = setFromList pcKeywords
      , treacheryAttachedLocation = Nothing
      , treacheryAttachedEnemy = Nothing
      , treacheryAttachedInvestigator = Nothing
      , treacheryOwner = iid
      , treacheryWeakness = True
      , treacheryResolved = False
      , treacheryDoom = 0
      , treacheryClues = Nothing
      , treacheryResources = Nothing
      }

instance HasActions env Attrs where
  getActions _ _ _ = pure []

is :: Target -> Attrs -> Bool
is (TreacheryTarget tid) t = tid == treacheryId t
is (CardCodeTarget cardCode) t = cardCode == treacheryCardCode t
is (CardIdTarget cardId) t = unCardId cardId == unTreacheryId (treacheryId t)
is _ _ = False

instance (TreacheryRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorEliminated iid | treacheryAttachedInvestigator == Just iid ->
      a <$ unshiftMessage (Discard (TreacheryTarget treacheryId))
    AttachTreachery tid (InvestigatorTarget iid) | tid == treacheryId ->
      pure $ a & attachedInvestigator ?~ iid
    AttachTreachery tid (LocationTarget lid) | tid == treacheryId ->
      pure $ a & attachedLocation ?~ lid
    AttachTreachery tid (EnemyTarget eid) | tid == treacheryId ->
      pure $ a & attachedEnemy ?~ eid
    AfterRevelation _iid tid | treacheryId == tid -> a <$ when
      treacheryResolved
      (unshiftMessage (Discard (TreacheryTarget treacheryId)))
    _ -> pure a
