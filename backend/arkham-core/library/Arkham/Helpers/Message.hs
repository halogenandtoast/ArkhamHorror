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
import Arkham.Message
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
