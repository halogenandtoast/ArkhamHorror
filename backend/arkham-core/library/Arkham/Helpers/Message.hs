module Arkham.Helpers.Message where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasQueue
import Arkham.Draw.Types
import Arkham.Exception
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..), WindowType )

drawCards
  :: (MonadRandom m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCards i source n = do
  drawing <- newCardDraw i source n
  pure $ DrawCards drawing

drawCardsAction
  :: (MonadRandom m, Sourceable source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCardsAction i source n = do
  drawing <- newCardDraw i source n
  pure $ DrawCards $ asDrawAction drawing

resolveWithWindow :: HasGame m => Message -> WindowType -> m [Message]
resolveWithWindow msg window' = do
  whenWindow <- checkWindows [Window Timing.When window']
  atIfWindow <- checkWindows [Window Timing.AtIf window']
  afterWindow <- checkWindows [Window Timing.After window']
  pure [When msg, whenWindow, atIfWindow, msg, After msg, afterWindow]

dealAdditionalDamage :: InvestigatorId -> Int -> [Message] -> GameT ()
dealAdditionalDamage iid amount additionalMessages = do
  mMsg <- findFromQueue $ \case
    InvestigatorDamage iid' _ n _ | iid' == iid -> n > 0
    InvestigatorDoAssignDamage iid' _ _ _ n _ [] [] | iid' == iid -> n > 0
    _ -> False
  case mMsg of
    Just damageMsg -> do
      let
        newMsg = case damageMsg of
          InvestigatorDamage _ source' n horror ->
            InvestigatorDamage iid source' (n + amount) horror
          InvestigatorDoAssignDamage _ source' strategy matcher n horror [] []
            -> InvestigatorDoAssignDamage
              iid
              source'
              strategy
              matcher
              (n + amount)
              horror
              []
              []
          _ -> error "impossible"
      replaceMessage damageMsg $ newMsg : additionalMessages
    Nothing -> throwIO $ InvalidState "No damage occured"

createEnemy :: MonadRandom m => Card -> m (EnemyId, Message)
createEnemy c = do
  enemyId <- getRandom
  pure (enemyId, CreateEnemy enemyId c)

createEnemy_ :: MonadRandom m => Card -> m Message
createEnemy_ = fmap snd . createEnemy

createEnemyWithPlacement :: MonadRandom m => Card -> Placement -> m (EnemyId, Message)
createEnemyWithPlacement c placement = do
  enemyId <- getRandom
  pure (enemyId, CreateEnemyWithPlacement enemyId c placement)

createEnemyWithPlacement_ :: MonadRandom m => Card -> Placement -> m Message
createEnemyWithPlacement_ c placement = snd <$> createEnemyWithPlacement c placement

createEnemyAt :: MonadRandom m => Card -> LocationId -> Maybe Target -> m (EnemyId, Message)
createEnemyAt c lid mTarget = do
  enemyId <- getRandom
  pure (enemyId, CreateEnemyAt enemyId c lid mTarget)

createEnemyAt_ :: MonadRandom m => Card -> LocationId -> Maybe Target -> m Message
createEnemyAt_ c lid mTarget = snd <$> createEnemyAt c lid mTarget

createEnemyAtLocationMatching :: MonadRandom m => Card -> LocationMatcher -> m (EnemyId, Message)
createEnemyAtLocationMatching c matcher = do
  enemyId <- getRandom
  pure (enemyId, CreateEnemyAtLocationMatching enemyId c matcher)

createEnemyAtLocationMatching_ :: MonadRandom m => Card -> LocationMatcher -> m Message
createEnemyAtLocationMatching_ c matcher = snd <$> createEnemyAtLocationMatching c matcher

createEnemyEngagedWithPrey :: MonadRandom m => Card -> m (EnemyId, Message)
createEnemyEngagedWithPrey c = do
  enemyId <- getRandom
  pure (enemyId, CreateEnemyEngagedWithPrey enemyId c)

createEnemyEngagedWithPrey_ :: MonadRandom m => Card -> m Message
createEnemyEngagedWithPrey_ = fmap snd . createEnemyEngagedWithPrey

placeLocation :: MonadRandom m => Card -> m (LocationId, Message)
placeLocation c = do
  locationId <- getRandom
  pure (locationId, PlaceLocation locationId c)

placeLocation_ :: MonadRandom m => Card -> m Message
placeLocation_ = fmap snd . placeLocation

placeSetAsideLocation :: CardDef -> GameT (LocationId, Message)
placeSetAsideLocation = placeLocation <=< getSetAsideCard

placeSetAsideLocation_ :: CardDef -> GameT Message
placeSetAsideLocation_ = placeLocation_ <=< getSetAsideCard

placeLocationCard :: CardDef -> GameT (LocationId, Message)
placeLocationCard = placeLocation <=< genCard

placeLocationCard_ :: CardDef -> GameT Message
placeLocationCard_ = placeLocation_ <=< genCard

scenarioResolution :: Int -> Message
scenarioResolution = ScenarioResolution . Resolution

toDiscard :: (Sourceable source, Targetable target) => source -> target -> Message
toDiscard source target = Discard (toSource source) (toTarget target)

pushAllM :: GameT [Message] -> GameT ()
pushAllM mmsgs = do
  msgs <- mmsgs
  pushAll msgs

pushM :: GameT Message -> GameT ()
pushM mmsg = do
  msg <- mmsg
  push msg
