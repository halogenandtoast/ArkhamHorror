{- | State and operations for auxiliary bags, not the skill-test chaos bag.
Owners decide when to open windows, resolve effects, and replenish the bag.
-}
module Arkham.TokenBag where

import Arkham.ChaosToken.Types
import Arkham.Prelude
import Data.Aeson (Result (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import GHC.Records

data BagToken = BagToken {bagTokenId :: ChaosTokenId, bagTokenFace :: ChaosTokenFace}
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass ToJSON

instance FromJSON BagToken where
  parseJSON = withObject "BagToken" \o ->
    BagToken
      <$> (o .: "bagTokenId" <|> o .: "infestationTokenId" <|> o .: "predationTokenId")
      <*> (o .: "bagTokenFace" <|> o .: "infestationTokenFace" <|> o .: "predationTokenFace")

instance HasField "face" BagToken ChaosTokenFace where
  getField = bagTokenFace

asChaosToken :: BagToken -> ChaosToken
asChaosToken (BagToken tokenId face) = ChaosToken tokenId face Nothing False False

type CustomChaosBag = TokenBag BagToken

initTokenBag :: MonadRandom m => [ChaosTokenFace] -> m CustomChaosBag
initTokenBag faces = do
  ids <- getRandoms
  pure $ newTokenBag $ zipWith BagToken ids faces

data TokenBag token = TokenBag
  { bagTokens :: [token]
  , bagSetAside :: [token]
  , bagCurrentToken :: Maybe token
  , bagCancelNext :: Bool
  , bagDebugNext :: Maybe ChaosTokenFace
  }
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass ToJSON

-- Accept the original story metadata, including Predation saves from before
-- the cancellation flag was introduced.
instance FromJSON token => FromJSON (TokenBag token) where
  parseJSON =
    withObject
      "TokenBag"
      ( \o -> do
          let prefix
                | KeyMap.member "bagTokens" o = "bag"
                | KeyMap.member "predationTokens" o = "predation"
                | otherwise = "infestation"
              key suffix = Key.fromText $ prefix <> suffix
          TokenBag
            <$> o
            .: key "Tokens"
            <*> o
            .: key "SetAside"
            <*> o
            .:? key "CurrentToken"
            <*> o
            .:? key "CancelNext"
            .!= False
            <*> o
            .:? "bagDebugNext"
      )

instance HasField "tokens" (TokenBag token) [token] where
  getField = bagTokens

instance HasField "setAside" (TokenBag token) [token] where
  getField = bagSetAside

instance HasField "currentToken" (TokenBag token) (Maybe token) where
  getField = bagCurrentToken

instance HasField "cancelNext" (TokenBag token) Bool where
  getField = bagCancelNext

newTokenBag :: [token] -> TokenBag token
newTokenBag tokens = TokenBag tokens [] Nothing False Nothing

allBagTokens :: TokenBag token -> [token]
allBagTokens bag = bag.tokens <> bag.setAside <> maybeToList bag.currentToken

{- | Draw without replacement. A debug override selects an existing token only,
is consumed once, and never bypasses the owner's normal reveal/effect path.
An unavailable override is cleared and the draw remains random.
-}
drawBagToken
  :: (MonadRandom m, Eq token)
  => (token -> ChaosTokenFace) -> TokenBag token -> m (Maybe token, TokenBag token)
drawBagToken face bag
  | isJust bag.currentToken = pure (Nothing, bag)
  | otherwise = do
      token <- case bagDebugNext bag >>= \wanted -> find ((== wanted) . face) bag.tokens of
        Just t -> pure $ Just t
        Nothing -> traverse sample (nonEmpty bag.tokens)
      pure
        ( token
        , bag
            { bagTokens = maybe bag.tokens (`deleteFirst` bag.tokens) token
            , bagCurrentToken = token
            , bagDebugNext = Nothing
            }
        )

setAsideBagToken :: TokenBag token -> TokenBag token
setAsideBagToken bag =
  bag {bagSetAside = bag.setAside <> maybeToList bag.currentToken, bagCurrentToken = Nothing}

returnBagToken :: TokenBag token -> TokenBag token
returnBagToken bag =
  bag {bagTokens = bag.tokens <> maybeToList bag.currentToken, bagCurrentToken = Nothing}

returnSetAsideTokens :: TokenBag token -> TokenBag token
returnSetAsideTokens bag = bag {bagTokens = bag.tokens <> bag.setAside, bagSetAside = []}

{- | Shared debug protocol for any owner storing a bag as JSON. Reject invalid
faces and unavailable tokens; clearing an override is always allowed.
-}

{- | Contents edits operate on the latest owner state, never a client snapshot.
The current reveal is deliberately immutable: queued effects may refer to it.
-}
editTokenBag :: MonadRandom m => Value -> Value -> m (Maybe Value)
editTokenBag choice value = case choice of
  Object command -> case fromJSON value of
    Error _ -> pure Nothing
    Success (bag :: CustomChaosBag) -> do
      let decodeValue v = case fromJSON v of
            Success a -> Just a
            Error _ -> Nothing
          selected pile = do
            tokenId <- KeyMap.lookup "id" command >>= decodeValue
            find ((== tokenId) . bagTokenId) pile
          finish b =
            Just
              $ toJSON
              $ b
                { bagDebugNext =
                    bagDebugNext b >>= \face ->
                      if any ((== face) . bagTokenFace) b.tokens then Just face else Nothing
                }
      case KeyMap.lookup "action" command of
        Just (String "add") | Just face <- KeyMap.lookup "face" command >>= decodeValue -> do
          tokenId <- getRandom
          pure $ finish bag {bagTokens = bag.tokens <> [BagToken tokenId face]}
        Just (String "remove")
          | Just token <- selected bag.tokens ->
              pure $ finish bag {bagTokens = deleteFirst token bag.tokens}
        Just (String "setAside")
          | Just token <- selected bag.tokens ->
              pure $ finish bag {bagTokens = deleteFirst token bag.tokens, bagSetAside = bag.setAside <> [token]}
        Just (String "return")
          | Just token <- selected bag.setAside ->
              pure $ finish bag {bagTokens = bag.tokens <> [token], bagSetAside = deleteFirst token bag.setAside}
        Just (String "returnSetAside") -> pure $ finish $ returnSetAsideTokens bag
        _ -> pure Nothing
  _ -> pure $ debugTokenBag choice value

debugTokenBag :: Value -> Value -> Maybe Value
debugTokenBag choice value = do
  bag <- case fromJSON value of
    Success b -> Just (b :: TokenBag Value)
    Error _ -> Nothing
  wanted <- case fromJSON choice of
    Success f -> Just (f :: Maybe ChaosTokenFace)
    Error _ -> Nothing
  let face token = case token of
        Object o -> asum $ map (`KeyMap.lookup` o) ["bagTokenFace", "infestationTokenFace", "predationTokenFace"]
        _ -> Just token
  guard $ maybe True (\f -> any ((== Just (toJSON f)) . face) bag.tokens) wanted
  pure $ toJSON bag {bagDebugNext = wanted}
