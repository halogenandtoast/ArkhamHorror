{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Attrs where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Keyword
import Arkham.Types.Treachery.Runner
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

data Attrs = Attrs
  { treacheryName :: Text
  , treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryTraits :: HashSet Trait
  , treacheryKeywords :: HashSet Keyword
  , treacheryAttachedLocation :: Maybe LocationId
  , treacheryAttachedInvestigator :: Maybe InvestigatorId
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

toSource :: Attrs -> Source
toSource Attrs { treacheryId } = TreacherySource treacheryId

isSource :: Attrs -> Source -> Bool
isSource Attrs { treacheryId } (TreacherySource tid) = treacheryId == tid
isSource _ _ = False

ownedBy :: Attrs -> InvestigatorId -> Bool
ownedBy Attrs { treacheryAttachedInvestigator } iid =
  treacheryAttachedInvestigator == Just iid

resolved :: Lens' Attrs Bool
resolved = lens treacheryResolved $ \m x -> m { treacheryResolved = x }

attachedLocation :: Lens' Attrs (Maybe LocationId)
attachedLocation =
  lens treacheryAttachedLocation $ \m x -> m { treacheryAttachedLocation = x }

attachedInvestigator :: Lens' Attrs (Maybe InvestigatorId)
attachedInvestigator = lens treacheryAttachedInvestigator
  $ \m x -> m { treacheryAttachedInvestigator = x }


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
      , treacheryTraits = HashSet.fromList ecTraits
      , treacheryKeywords = HashSet.fromList ecKeywords
      , treacheryAttachedLocation = Nothing
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
    AttachTreachery tid (LocationTarget lid) | tid == treacheryId ->
      pure $ a & attachedLocation ?~ lid
    AfterRevelation{} -> a <$ when
      treacheryResolved
      (unshiftMessage (Discard (TreacheryTarget treacheryId)))
    _ -> pure a
