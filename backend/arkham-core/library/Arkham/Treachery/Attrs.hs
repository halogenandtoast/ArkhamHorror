module Arkham.Treachery.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Id
import Arkham.Json
import Arkham.Keyword
import Arkham.Name
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Cards

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ TreacheryId, EntityAttrs a ~ TreacheryAttrs) => IsTreachery a

type TreacheryCard a = CardBuilder (InvestigatorId, TreacheryId) a

data instance Field TreacheryAttrs :: Type -> Type where
  TreacheryClues :: Field TreacheryAttrs Int
  TreacheryResources :: Field TreacheryAttrs Int
  TreacheryDoom :: Field TreacheryAttrs Int
  TreacheryAttachedTarget :: Field TreacheryAttrs (Maybe Target)
  TreacheryTraits :: Field TreacheryAttrs (HashSet Trait)
  TreacheryKeywords :: Field TreacheryAttrs (HashSet Keyword)
  TreacheryAbilities :: Field TreacheryAttrs [Ability]
  TreacheryCardDef :: Field TreacheryAttrs CardDef
  TreacheryCard :: Field TreacheryAttrs Card
  TreacheryCanBeCommitted :: Field TreacheryAttrs Bool

data TreacheryAttrs = TreacheryAttrs
  { treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryAttachedTarget :: Maybe Target
  , treacheryOwner :: Maybe InvestigatorId
  , treacheryInHandOf :: Maybe InvestigatorId
  , treacheryDoom :: Int
  , treacheryClues :: Int
  , treacheryResources :: Int
  , treacheryCanBeCommitted :: Bool
  }
  deriving stock (Show, Eq, Generic)

cluesL :: Lens' TreacheryAttrs Int
cluesL = lens treacheryClues $ \m x -> m { treacheryClues = x }

attachedTargetL :: Lens' TreacheryAttrs (Maybe Target)
attachedTargetL =
  lens treacheryAttachedTarget $ \m x -> m { treacheryAttachedTarget = x }

inHandOfL :: Lens' TreacheryAttrs (Maybe InvestigatorId)
inHandOfL = lens treacheryInHandOf $ \m x -> m { treacheryInHandOf = x }

resourcesL :: Lens' TreacheryAttrs Int
resourcesL = lens treacheryResources $ \m x -> m { treacheryResources = x }

canBeCommittedL :: Lens' TreacheryAttrs Bool
canBeCommittedL = lens treacheryCanBeCommitted $ \m x -> m { treacheryCanBeCommitted = x }

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

instance Entity TreacheryAttrs where
  type EntityId TreacheryAttrs = TreacheryId
  type EntityAttrs TreacheryAttrs = TreacheryAttrs
  toId = treacheryId
  toAttrs = id
  overAttrs f = f

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
  isSource _ _ = False

instance IsCard TreacheryAttrs where
  toCardId = unTreacheryId . treacheryId
  toCardOwner = treacheryOwner

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

withTreacheryEnemy :: TreacheryAttrs -> (EnemyId -> m a) -> m a
withTreacheryEnemy attrs f = case treacheryAttachedTarget attrs of
  Just (EnemyTarget eid) -> f eid
  _ ->
    error $ show (cdName $ toCardDef attrs) <> " must be attached to an enemy"

withTreacheryLocation :: TreacheryAttrs -> (LocationId -> m a) -> m a
withTreacheryLocation attrs f = case treacheryAttachedTarget attrs of
  Just (LocationTarget lid) -> f lid
  _ ->
    error $ show (cdName $ toCardDef attrs) <> " must be attached to a location"

withTreacheryInvestigator :: TreacheryAttrs -> (InvestigatorId -> m a) -> m a
withTreacheryInvestigator attrs f = case treacheryAttachedTarget attrs of
  Just (InvestigatorTarget iid) -> f iid
  _ ->
    error
      $ show (cdName $ toCardDef attrs)
      <> " must be attached to an investigator"

withTreacheryOwner :: TreacheryAttrs -> (InvestigatorId -> m a) -> m a
withTreacheryOwner attrs f = case treacheryOwner attrs of
  Just iid -> f iid
  _ ->
    error
      $ show (cdName $ toCardDef attrs)
      <> " must be owned by an investigator"

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
    , treacheryInHandOf = Nothing
    , treacheryOwner = if isJust (cdCardSubType cardDef)
      then Just iid
      else Nothing
    , treacheryDoom = 0
    , treacheryClues = 0
    , treacheryResources = 0
    , treacheryCanBeCommitted = False
    }
  }

is :: Target -> TreacheryAttrs -> Bool
is (TreacheryTarget tid) t = tid == treacheryId t
is (CardCodeTarget cardCode) t = cardCode == cdCardCode (toCardDef t)
is (CardIdTarget cardId) t = cardId == unTreacheryId (treacheryId t)
is _ _ = False
