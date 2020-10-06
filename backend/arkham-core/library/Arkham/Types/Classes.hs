{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Classes
  ( module Arkham.Types.Classes
  , module Arkham.Types.Classes.HasRecord
  )
where

import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.Classes.HasRecord
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.Keyword
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Token (Token, TokenValue(..))
import Arkham.Types.Trait
import Arkham.Types.Window (Window)
import qualified Arkham.Types.Window as Window
import ClassyPrelude
import Control.Monad.Fail
import qualified Data.HashSet as HashSet
import GHC.Generics
import GHC.Stack
import Lens.Micro hiding (to)
import Lens.Micro.Extras

class HasQueue a where
  messageQueue :: Lens' a (IORef [Message])

class (HasQueue env) => RunMessage1 env f where
  runMessage1 :: (MonadIO m, MonadReader env m, MonadFail m) => Message -> f p -> m (f p)

instance (HasQueue env, RunMessage1 env f) => RunMessage1 env (M1 i c f) where
  runMessage1 msg (M1 x) = M1 <$> runMessage1 msg x

instance (HasQueue env, RunMessage1 env l, RunMessage1 env r) => RunMessage1 env (l :+: r) where
  runMessage1 msg (L1 x) = L1 <$> runMessage1 msg x
  runMessage1 msg (R1 x) = R1 <$> runMessage1 msg x

instance (HasQueue env, RunMessage env p) => RunMessage1 env (K1 R p) where
  runMessage1 msg (K1 x) = K1 <$> runMessage msg x

class (HasQueue env) => RunMessage env a where
  runMessage :: (MonadIO m, MonadReader env m, MonadFail m) => Message -> a -> m a
  default runMessage :: (Generic a, RunMessage1 env (Rep a), MonadIO m, MonadReader env m, MonadFail m) => Message -> a -> m a
  runMessage = defaultRunMessage

defaultRunMessage
  :: ( Generic a
     , RunMessage1 env (Rep a)
     , MonadIO m
     , MonadReader env m
     , MonadFail m
     )
  => Message
  -> a
  -> m a
defaultRunMessage msg = fmap to . runMessage1 msg . from

withQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> ([Message], r))
  -> m r
withQueue body = do
  ref <- asks $ view messageQueue
  liftIO $ atomicModifyIORef' ref body

popMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
popMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: (MonadIO m, MonadReader env m, HasQueue env) => m ()
clearQueue = withQueue $ const ([], ())

peekMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
peekMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

pushMessage :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
pushMessage = pushMessages . pure

pushMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
pushMessages msgs = withQueue $ \queue -> (queue <> msgs, ())

unshiftMessage
  :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
unshiftMessage = unshiftMessages . pure

unshiftMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
unshiftMessages msgs = withQueue $ \queue -> (msgs <> queue, ())

pairInvestigatorIdsForWindow
  :: ( MonadReader env m
     , HasSet InvestigatorId () env
     , HasSet ConnectedLocationId LocationId env
     , HasId LocationId InvestigatorId env
     )
  => InvestigatorId
  -> m [(InvestigatorId, Window.Who)]
pairInvestigatorIdsForWindow iid = do
  investigatorIds <- setToList <$> asks (getSet @InvestigatorId ())
  lid <- asks (getId iid)
  connectedLocationIds <- HashSet.map unConnectedLocationId
    <$> asks (getSet lid)
  for investigatorIds $ \iid2 -> do
    lid2 <- asks (getId iid2)
    pure $ if iid2 == iid
      then (iid2, Window.You)
      else if lid2 == lid
        then (iid2, Window.InvestigatorAtYourLocation)
        else if lid2 `member` connectedLocationIds
          then (iid2, Window.InvestigatorAtAConnectedLocation)
          else (iid2, Window.InvestigatorInGame)

runTest
  :: ( HasQueue env
     , MonadReader env m
     , MonadIO m
     , HasSet InvestigatorId () env
     , HasSet ConnectedLocationId LocationId env
     , HasId LocationId InvestigatorId env
     )
  => InvestigatorId
  -> TokenValue
  -> m ()
runTest iid tokenValue@(TokenValue token value) = do
  windowPairings <- pairInvestigatorIdsForWindow iid
  if value < 0
    then unshiftMessages
      ([ CheckWindow
           iid'
           [ Window.WhenRevealTokenWithNegativeModifier who
           , Window.WhenRevealToken who token
           ]
       | (iid', who) <- windowPairings
       ]
      <> [When (RunSkillTest iid tokenValue), RunSkillTest iid tokenValue]
      )
    else unshiftMessages
      ([ CheckWindow iid' [Window.WhenRevealToken who token]
       | (iid', who) <- windowPairings
       ]
      <> [RunSkillTest iid tokenValue]
      )

class HasModifiers1 env f where
  getModifiers1 :: (MonadIO m, MonadReader env m) => f p -> m [Modifier]

instance HasModifiers1 env f => HasModifiers1 env (M1 i c f) where
  getModifiers1 (M1 x) = getModifiers1 x

instance (HasModifiers1 env l, HasModifiers1 env r) => HasModifiers1 env (l :+: r) where
  getModifiers1 (L1 x) = getModifiers1 x
  getModifiers1 (R1 x) = getModifiers1 x

instance (HasModifiers env p) => HasModifiers1 env (K1 R p) where
  getModifiers1 (K1 x) = getModifiers x

defaultGetModifiers
  :: (Generic a, HasModifiers1 env (Rep a), MonadIO m, MonadReader env m)
  => a
  -> m [Modifier]
defaultGetModifiers = getModifiers1 . from

class HasModifiers env a where
  getModifiers :: (MonadReader env m, MonadIO m) => a -> m [Modifier]
  default getModifiers :: (Generic a, HasModifiers1 env (Rep a), MonadIO m, MonadReader env m) => a -> m [Modifier]
  getModifiers = defaultGetModifiers

class HasSet c b a where
  getSet :: b -> a -> HashSet c

class HasList c b a where
  getList :: b -> a -> [c]

class HasId c b a where
  getId :: HasCallStack => b -> a -> c

class HasCount c b a where
  getCount :: b -> a -> c

class HasInvestigatorStats c b a where
  getStats :: b -> a -> c

class HasTraits a where
  getTraits :: a -> HashSet Trait

class IsAdvanceable a where
  isAdvanceable :: a -> Bool

class HasSkill a where
  getSkill :: SkillType -> a -> Int

class HasKeywords a where
  getKeywords :: a -> HashSet Keyword

class HasAbilities a where
  getAbilities :: a -> [Ability]

class HasVictoryPoints a where
  getVictoryPoints :: a -> Maybe Int

class HasDamage a where
  getDamage :: a -> (Int, Int)

class HasTrauma a where
  getTrauma :: a -> (Int, Int)

instance HasVictoryPoints Card where
  getVictoryPoints (PlayerCard card) = getVictoryPoints card
  getVictoryPoints (EncounterCard card) = getVictoryPoints card

instance HasVictoryPoints EncounterCard where
  getVictoryPoints MkEncounterCard {..} = ecVictoryPoints

instance HasVictoryPoints PlayerCard where
  getVictoryPoints = pcVictoryPoints . playerCardAttrs

type ActionRunner env investigator
  = ( IsInvestigator investigator
    , HasActions env investigator (ActionType, env)
    , HasId (Maybe StoryAssetId) CardCode env
    , HasId (Maybe OwnerId) AssetId env
    , HasId (Maybe LocationId) AssetId env
    , HasId LocationId InvestigatorId env
    , HasSet InvestigatorId LocationId env
    , HasSet ConnectedLocationId LocationId env
    , HasSet EnemyId LocationId env
    , HasList UsedAbility () env
    , HasCount PlayerCount () env
    , HasCount SpendableClueCount AllInvestigators env
    , HasCount ClueCount LocationId env
    , HasCount HorrorCount InvestigatorId env
    , HasQueue env
    , HasSet ExhaustedAssetId InvestigatorId env
    , HasModifiersFor env InvestigatorId env
    )

class HasActions1 env investigator f where
  getActions1 :: (MonadIO m, MonadReader env m) => investigator -> Window -> f p -> m [Message]

instance HasActions1 env investigator f => HasActions1 env investigator (M1 i c f) where
  getActions1 investigator window (M1 x) = getActions1 investigator window x

instance (HasActions1 env investigator l, HasActions1 env investigator r) => HasActions1 env investigator (l :+: r) where
  getActions1 investigator window (L1 x) = getActions1 investigator window x
  getActions1 investigator window (R1 x) = getActions1 investigator window x

instance (HasActions env investigator p) => HasActions1 env investigator (K1 R p) where
  getActions1 investigator window (K1 x) = getActions investigator window x

defaultGetActions
  :: ( Generic a
     , HasActions1 env investigator (Rep a)
     , MonadIO m
     , MonadReader env m
     )
  => investigator
  -> Window
  -> a
  -> m [Message]
defaultGetActions investigator window = getActions1 investigator window . from

class HasActions env investigator a where
  getActions :: (MonadReader env m, MonadIO m) => investigator -> Window -> a -> m [Message]
  default getActions :: (Generic a, HasActions1 env investigator (Rep a), MonadIO m, MonadReader env m) => investigator -> Window -> a -> m [Message]
  getActions = defaultGetActions

class HasModifiersFor1 env investigator f where
  getModifiersFor1 :: (MonadIO m, MonadReader env m) => investigator -> f p -> m [Modifier]

instance HasModifiersFor1 env investigator f => HasModifiersFor1 env investigator (M1 i c f) where
  getModifiersFor1 investigator (M1 x) = getModifiersFor1 investigator x

instance (HasModifiersFor1 env investigator l, HasModifiersFor1 env investigator r) => HasModifiersFor1 env investigator (l :+: r) where
  getModifiersFor1 investigator (L1 x) = getModifiersFor1 investigator x
  getModifiersFor1 investigator (R1 x) = getModifiersFor1 investigator x

instance (HasModifiersFor env investigator p) => HasModifiersFor1 env investigator (K1 R p) where
  getModifiersFor1 investigator (K1 x) = getModifiersFor investigator x

defaultGetModifiersFor
  :: ( Generic a
     , HasModifiersFor1 env investigator (Rep a)
     , MonadIO m
     , MonadReader env m
     )
  => investigator
  -> a
  -> m [Modifier]
defaultGetModifiersFor investigator = getModifiersFor1 investigator . from

class HasModifiersFor env investigator a where
  getModifiersFor :: (MonadReader env m, MonadIO m) => investigator -> a -> m [Modifier]
  default getModifiersFor :: (Generic a, HasModifiersFor1 env investigator (Rep a), MonadIO m, MonadReader env m) => investigator -> a -> m [Modifier]
  getModifiersFor = defaultGetModifiersFor

class (HasId LocationId () location) => IsLocation location where
  isBlocked :: location -> Bool

class (HasId EnemyId () enemy) => IsEnemy enemy where
  isAloof :: enemy -> Bool

class IsScenario scenario where
  tokensWithNegativeModifier :: scenario -> HashSet Token

class Exhaustable a where
  isExhausted :: a -> Bool
  isReady :: a -> Bool

  isExhausted = not . isReady
  isReady = not . isExhausted
  {-# MINIMAL isExhausted | isReady #-}

class (HasId InvestigatorId () investigator) => IsInvestigator investigator where
  locationOf :: investigator -> LocationId
  canInvestigate :: (IsLocation location) => location -> investigator -> Bool
  canMoveTo :: (IsLocation location) => location -> investigator -> Bool
  canFight :: (IsEnemy enemy) => enemy -> investigator -> Bool
  canEngage :: (IsEnemy enemy) => enemy -> investigator -> Bool
  canEvade :: (IsEnemy enemy) => enemy -> investigator -> Bool
  remainingSanity :: investigator -> Int
  remainingHealth :: investigator -> Int
  resourceCount :: investigator -> Int
  clueCount :: investigator -> Int
  spendableClueCount :: investigator -> Int
  cardCount :: investigator -> Int
  discardableCardCount :: investigator -> Int
  canDo :: Action -> investigator -> Bool
  hasActionsRemaining :: investigator -> Maybe Action -> HashSet Trait -> Bool
  canTakeDirectDamage :: investigator -> Bool
  discardOf :: investigator -> [PlayerCard]
  handOf :: investigator -> [Card]
  deckOf :: investigator -> [PlayerCard]
