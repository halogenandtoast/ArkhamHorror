module Main (main) where

import AH3e.Engine
import AH3e.Game
import AH3e.Prelude hiding ((.=))
import AH3e.Types.Card
import AH3e.Types.Ids
import AH3e.Types.State
import AH3e.View (catalogView, gameView)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.IORef
import Data.Text qualified as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data NewGame = NewGame {seed :: Int, players :: Int, expansions :: [Expansion], mode :: GameMode}
  deriving stock Generic
  deriving anyclass FromJSON

data Answer = Answer {player :: Int, choice :: Int}
  deriving stock Generic
  deriving anyclass FromJSON

main :: IO ()
main = do
  port <- fromMaybe 3100 . (>>= readMaybe) <$> lookupEnv "PORT"
  viewer <- fromMaybe "dev/viewer.html" <$> lookupEnv "AH3E_VIEWER"
  publicDir <- fromMaybe "../../frontend-3ed/public" <$> lookupEnv "AH3E_PUBLIC"
  ref <- newIORef Nothing
  history <- newIORef []
  putStrLn ("ah3e dev viewer on http://localhost:" <> show port)
  run port (app publicDir viewer ref history)

start :: IORef (Maybe Game) -> NewGame -> IO (Either Text ())
start ref ng = do
  let opts = GameOptions {expansions = ng.expansions, mode = ng.mode, debug = True}
  case newGame [PlayerId n | n <- [1 .. ng.players]] ng.seed opts of
    Left e -> pure (Left e)
    Right g -> case runEngine g of
      Left e -> pure (Left (tshow e))
      Right g' -> Right () <$ writeIORef ref (Just g')

-- earlier states, newest first; each answer or debug action saves the state it replaced
historyLimit :: Int
historyLimit = 500

app :: FilePath -> FilePath -> IORef (Maybe Game) -> IORef [Game] -> Application
app publicDir viewer ref history req respond = case (requestMethod req, pathInfo req) of
  ("GET", []) ->
    respond
      ( responseFile
          status200
          [(hContentType, "text/html; charset=utf-8"), ("Cache-Control", "no-store")]
          viewer
          Nothing
      )
  ("GET", dir : rest)
    | dir `elem` ["img", "fonts"]
    , all (\p -> not (T.null p) && p /= ".." && p /= ".") rest -> do
        let path = publicDir <> "/" <> T.unpack dir <> "/" <> T.unpack (T.intercalate "/" rest)
        exists <- doesFileExist path
        if exists
          then
            respond
              ( responseFile
                  status200
                  [(hContentType, mimeFor path), ("Cache-Control", "max-age=3600")]
                  path
                  Nothing
              )
          else respond (responseLBS status404 [] "not found")
  ("GET", ["api", "state"]) -> ok
  ("POST", ["api", "reset"]) -> writeIORef ref Nothing >> writeIORef history [] >> ok
  ("POST", ["api", "new"]) -> withBody \(ng :: NewGame) -> writeIORef history [] >> start ref ng >>= either failWith (const ok)
  ("POST", ["api", "undo"]) ->
    readIORef history >>= \case
      [] -> failWith "nothing to undo"
      (prev : rest) -> writeIORef history rest >> writeIORef ref (Just prev) >> ok
  ("POST", ["api", "answer"]) -> withBody \(a :: Answer) -> step (answer (PlayerId a.player) a.choice)
  ("POST", ["api", "debug"]) -> withBody \(d :: DebugAction) -> step (applyDebug d)
  _ -> respond (responseLBS status404 [] "not found")
 where
  withBody :: FromJSON a => (a -> IO ResponseReceived) -> IO ResponseReceived
  withBody k = do
    body <- strictRequestBody req
    either (failWith . tshow) k (Aeson.eitherDecode body)
  step f =
    readIORef ref >>= \case
      Nothing -> failWith "no game"
      Just g -> case f g of
        Left e -> failWith (tshow e)
        Right g' -> do
          modifyIORef' history (take historyLimit . (g :))
          writeIORef ref (Just g')
          ok
  ok = do
    canUndo <- not . null <$> readIORef history
    readIORef ref >>= respond . stateResponse canUndo
  failWith e =
    respond
      (responseLBS status400 [(hContentType, "application/json")] (Aeson.encode (object ["error" .= e])))

stateResponse :: Bool -> Maybe Game -> Response
stateResponse canUndo mg =
  responseLBS status200 [(hContentType, "application/json")]
    $ Aeson.encode
    $ object
    $ ["canUndo" .= canUndo]
    <> catalogView
    <> case gameView <$> mg of
      Just (Aeson.Object o) -> KeyMap.toList o
      _ -> []

mimeFor :: FilePath -> ByteString
mimeFor path = case reverse (takeWhile (/= '.') (reverse path)) of
  "webp" -> "image/webp"
  "png" -> "image/png"
  "jpg" -> "image/jpeg"
  "jpeg" -> "image/jpeg"
  "avif" -> "image/avif"
  "ttf" -> "font/ttf"
  _ -> "application/octet-stream"
