module Arkham.Types.Treachery.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Treachery.Cards
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
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

class IsTreachery a

type TreacheryCard a = CardBuilder (InvestigatorId, TreacheryId) a

data TreacheryAttrs = TreacheryAttrs
  { treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryAttachedTarget :: Maybe Target
  , treacheryOwner :: Maybe InvestigatorId
  , treacheryDoom :: Int
  , treacheryClues :: Maybe Int
  , treacheryResources :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

cluesL :: Lens' TreacheryAttrs (Maybe Int)
cluesL = lens treacheryClues $ \m x -> m { treacheryClues = x }

attachedTargetL :: Lens' TreacheryAttrs (Maybe Target)
attachedTargetL =
  lens treacheryAttachedTarget $ \m x -> m { treacheryAttachedTarget = x }

resourcesL :: Lens' TreacheryAttrs (Maybe Int)
resourcesL = lens treacheryResources $ \m x -> m { treacheryResources = x }

instance HasCardCode TreacheryAttrs where
  toCardCode = treacheryCardCode

instance HasCardDef TreacheryAttrs where
  toCardDef a = case lookup (treacheryCardCode a) allTreacheryCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for treachery " <> show (treacheryCardCode a)

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

instance Named TreacheryAttrs where
  toName = toName . toCardDef

instance TargetEntity TreacheryAttrs where
  toTarget = TreacheryTarget . toId
  isTarget TreacheryAttrs { treacheryId } (TreacheryTarget tid) =
    treacheryId == tid
  isTarget _ _ = False

instance SourceEntity TreacheryAttrs where
  toSource = TreacherySource . toId
  isSource TreacheryAttrs { treacheryId } (TreacherySource tid) =
    treacheryId == tid
  isSource attrs (PlayerCardSource cardId) = toCardId attrs == cardId
  isSource _ _ = False

instance IsCard TreacheryAttrs where
  toCardId = unTreacheryId . treacheryId

instance DiscardableEntity TreacheryAttrs

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
    (InvalidState
    $ tshow (cdName $ toCardDef attrs)
    <> " must be attached to an enemy"
    )

withTreacheryLocation
  :: MonadIO m => TreacheryAttrs -> (LocationId -> m a) -> m a
withTreacheryLocation attrs f = case treacheryAttachedTarget attrs of
  Just (LocationTarget lid) -> f lid
  _ -> throwIO
    (InvalidState
    $ tshow (cdName $ toCardDef attrs)
    <> " must be attached to a location"
    )

withTreacheryInvestigator
  :: MonadIO m => TreacheryAttrs -> (InvestigatorId -> m a) -> m a
withTreacheryInvestigator attrs f = case treacheryAttachedTarget attrs of
  Just (InvestigatorTarget iid) -> f iid
  _ -> throwIO
    (InvalidState
    $ tshow (cdName $ toCardDef attrs)
    <> " must be attached to an investigator"
    )

treachery
  :: (TreacheryAttrs -> a)
  -> CardDef
  -> CardBuilder (InvestigatorId, TreacheryId) a
treachery f cardDef = treacheryWith f cardDef id

treacheryWith
  :: (TreacheryAttrs -> a)
  -> CardDef
  -> (TreacheryAttrs -> TreacheryAttrs)
  -> CardBuilder (InvestigatorId, TreacheryId) a
treacheryWith f cardDef g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(iid, tid) -> f . g $ TreacheryAttrs
    { treacheryId = tid
    , treacheryCardCode = toCardCode cardDef
    , treacheryAttachedTarget = Nothing
    , treacheryOwner = if cdWeakness cardDef then Just iid else Nothing
    , treacheryDoom = 0
    , treacheryClues = Nothing
    , treacheryResources = Nothing
    }
  }

is :: Target -> TreacheryAttrs -> Bool
is (TreacheryTarget tid) t = tid == treacheryId t
is (CardCodeTarget cardCode) t = cardCode == cdCardCode (toCardDef t)
is (CardIdTarget cardId) t = cardId == unTreacheryId (treacheryId t)
is _ _ = False

instance TreacheryRunner env => RunMessage env TreacheryAttrs where
  runMessage msg a@TreacheryAttrs {..} = case msg of
    InvestigatorEliminated iid
      | InvestigatorTarget iid `elem` treacheryAttachedTarget -> a
      <$ push (Discard $ toTarget a)
    AttachTreachery tid target | tid == treacheryId ->
      pure $ a & attachedTargetL ?~ target
    PlaceResources target n | isTarget a target -> do
      let amount = fromMaybe 0 treacheryResources + n
      pure $ a & resourcesL ?~ amount
    PlaceEnemyInVoid eid | EnemyTarget eid `elem` treacheryAttachedTarget ->
      a <$ push (Discard $ toTarget a)
    Discard target | target `elem` treacheryAttachedTarget ->
      a <$ push (Discard $ toTarget a)
    _ -> pure a
