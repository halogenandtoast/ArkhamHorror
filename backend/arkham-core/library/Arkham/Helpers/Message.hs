module Arkham.Helpers.Message where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Draw.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..), WindowType )

drawCards
  :: (MonadRandom m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCards i source n = do
  drawing <- newCardDraw i source n
  pure $ DrawCards drawing

drawCardsAction
  :: (MonadRandom m, SourceEntity source)
  => InvestigatorId
  -> source
  -> Int
  -> m Message
drawCardsAction i source n = do
  drawing <- newCardDraw i source n
  pure $ DrawCards $ asDrawAction drawing

resolveWithWindow
  :: HasGame m => Message -> WindowType -> m [Message]
resolveWithWindow msg window' = do
  whenWindow <- checkWindows [Window Timing.When window']
  atIfWindow <- checkWindows [Window Timing.AtIf window']
  afterWindow <- checkWindows [Window Timing.After window']
  pure $ [When msg, whenWindow, atIfWindow, msg, After msg, afterWindow]
