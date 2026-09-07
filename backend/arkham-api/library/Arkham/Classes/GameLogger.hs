module Arkham.Classes.GameLogger where

import Arkham.Card.Id
import Arkham.Id
import Arkham.Prelude
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

class MonadIO m => HasGameLogger m where
  getLogger :: m (ClientMessage -> IO ())

instance HasGameLogger m => HasGameLogger (ReaderT e m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

instance HasGameLogger m => HasGameLogger (StateT s m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

instance (Monoid w, HasGameLogger m) => HasGameLogger (WriterT w m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

class ToGameLoggerFormat a where
  format :: a -> Text

formatAsSentence :: ToGameLoggerFormat a => [a] -> Text
formatAsSentence = go False
 where
  go _ [] = ""
  go _ [a] = format a
  go True [a, b] = format a <> ", and " <> format b
  go False [a, b] = format a <> " and " <> format b
  go _ (a : as) = format a <> ", " <> go True as

data ClientMessage
  = ClientText Text
  | ClientError Text
  | ClientCard Text Value
  | ClientCardOnly PlayerId Text Value
  | ClientTarot Value
  | ClientShowDiscard InvestigatorId
  | ClientShowUnder InvestigatorId
  | ClientUI Text
  | ClientAudio Text
  | ClientPlayabilityReport CardId Text [(Text, Maybe Text)]
  | {- | A custom card's JSON did not do what it said. Carries enough to fix it:
    which card, what went wrong, and the offending fragment. Never thrown --
    the card simply did nothing -- so without this the author sees silence.
    -}
    ClientCustomCardIssue Text Text Value

send :: HasGameLogger m => Text -> m ()
send msg = do
  f <- getLogger
  liftIO $ f (ClientText msg)

sendUI :: HasGameLogger m => Text -> m ()
sendUI msg = do
  f <- getLogger
  liftIO $ f (ClientUI msg)

sendAudio :: HasGameLogger m => Text -> m ()
sendAudio fileName = do
  f <- getLogger
  liftIO $ f (ClientAudio fileName)

{- | Report a custom card whose JSON could not be used, to whoever is looking at
the game. Silent failure is the wrong default for something a person is in the
middle of authoring.
-}
sendCustomCardIssue :: HasGameLogger m => Text -> Text -> Value -> m ()
sendCustomCardIssue cardCode detail payload = do
  f <- getLogger
  liftIO $ f (ClientCustomCardIssue cardCode detail payload)

sendError :: HasGameLogger m => Text -> m ()
sendError msg = do
  f <- getLogger
  liftIO $ f (ClientError msg)

sendRevelation :: HasGameLogger m => PlayerId -> Value -> m ()
sendRevelation pid msg = do
  f <- getLogger
  liftIO $ f (ClientCardOnly pid "Revelation" msg)

sendReveal :: HasGameLogger m => Value -> m ()
sendReveal msg = do
  f <- getLogger
  liftIO $ f (ClientCard "Revealed" msg)

sendEnemy :: HasGameLogger m => Text -> Value -> m ()
sendEnemy title msg = do
  f <- getLogger
  liftIO $ f (ClientCard title msg)

sendEnemyOnly :: HasGameLogger m => PlayerId -> Text -> Value -> m ()
sendEnemyOnly pid title msg = do
  f <- getLogger
  liftIO $ f (ClientCardOnly pid title msg)

sendTarot :: HasGameLogger m => Value -> m ()
sendTarot msg = do
  f <- getLogger
  liftIO $ f (ClientTarot msg)
sendShowUnder :: HasGameLogger m => InvestigatorId -> m ()
sendShowUnder iid = do
  f <- getLogger
  liftIO $ f (ClientShowUnder iid)
