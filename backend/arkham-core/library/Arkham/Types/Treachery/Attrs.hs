{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Treachery.Attrs where

import Arkham.Import

import Arkham.Types.Trait
import Arkham.Types.Keyword
import Arkham.Types.Treachery.Runner
import qualified Data.HashMap.Strict as HashMap

data Attrs = Attrs
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
  deriving stock (Show, Generic)

makeLensesWith suffixedFields ''Attrs

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "treachery"
  toEncoding = genericToEncoding $ aesonOptions $ Just "treachery"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "treachery"

instance HasCount ResourceCount env Attrs where
  getCount = pure . ResourceCount . fromMaybe 0 . treacheryResources

instance Entity Attrs where
  type EntityId Attrs = TreacheryId
  type EntityAttrs Attrs = Attrs
  toId = treacheryId
  toAttrs = id
  toName = mkName . treacheryName

instance TargetEntity Attrs where
  toTarget = TreacheryTarget . toId
  isTarget Attrs { treacheryId } (TreacheryTarget tid) = treacheryId == tid
  isTarget _ _ = False

instance SourceEntity Attrs where
  toSource = TreacherySource . toId
  isSource Attrs { treacheryId } (TreacherySource tid) = treacheryId == tid
  isSource _ _ = False

instance IsCard Attrs where
  getCardId = CardId . unTreacheryId . treacheryId
  getCardCode = treacheryCardCode
  getTraits = treacheryTraits
  getKeywords = treacheryKeywords

-- ownedBy :: Attrs -> InvestigatorId -> Bool
-- ownedBy Attrs { treacheryOwner } iid = treacheryOwner == Just iid

treacheryOn :: Target -> Attrs -> Bool
treacheryOn t Attrs { treacheryAttachedTarget } =
  t `elem` treacheryAttachedTarget

treacheryOnInvestigator :: InvestigatorId -> Attrs -> Bool
treacheryOnInvestigator = treacheryOn . InvestigatorTarget

treacheryOnEnemy :: EnemyId -> Attrs -> Bool
treacheryOnEnemy = treacheryOn . EnemyTarget

treacheryOnLocation :: LocationId -> Attrs -> Bool
treacheryOnLocation = treacheryOn . LocationTarget

withTreacheryEnemy :: MonadIO m => Attrs -> (EnemyId -> m a) -> m a
withTreacheryEnemy attrs f = case treacheryAttachedTarget attrs of
  Just (EnemyTarget eid) -> f eid
  _ -> throwIO
    (InvalidState $ treacheryName attrs <> " must be attached to an enemy")

withTreacheryLocation :: MonadIO m => Attrs -> (LocationId -> m a) -> m a
withTreacheryLocation attrs f = case treacheryAttachedTarget attrs of
  Just (LocationTarget lid) -> f lid
  _ -> throwIO
    (InvalidState $ treacheryName attrs <> " must be attached to a location")

withTreacheryInvestigator
  :: MonadIO m => Attrs -> (InvestigatorId -> m a) -> m a
withTreacheryInvestigator attrs f = case treacheryAttachedTarget attrs of
  Just (InvestigatorTarget iid) -> f iid
  _ -> throwIO
    (InvalidState
    $ treacheryName attrs
    <> " must be attached to an investigator"
    )

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
      , treacheryKeywords = ecKeywords
      , treacheryAttachedTarget = Nothing
      , treacheryOwner = Nothing
      , treacheryWeakness = False
      , treacheryDoom = 0
      , treacheryClues = Nothing
      , treacheryResources = Nothing
      }

weaknessAttrs
  :: HasCallStack => TreacheryId -> Maybe InvestigatorId -> CardCode -> Attrs
weaknessAttrs tid iid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing weakness card: " <> show cardCode)
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unTreacheryId tid)
  in
    Attrs
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

instance HasActions env Attrs where
  getActions _ _ _ = pure []

is :: Target -> Attrs -> Bool
is (TreacheryTarget tid) t = tid == treacheryId t
is (CardCodeTarget cardCode) t = cardCode == treacheryCardCode t
is (CardIdTarget cardId) t = unCardId cardId == unTreacheryId (treacheryId t)
is _ _ = False

instance TreacheryRunner env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
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
