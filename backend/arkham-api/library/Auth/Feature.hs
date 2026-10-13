{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Per-account feature grants: which of the not-yet-general features an
account is allowed to reach.

Not the same question as an API key's scopes in "Auth.ApiKey", and deliberately
not the same mechanism. A scope narrows one credential to less than its account
can already do; a feature decides what the account can do at all. So a scope is
chosen by whoever mints the key, and a feature is granted by an admin -- a key
cannot grant itself one, because nothing it can reach writes this column.

Also not 'userBeta', which the owner of the account sets for themselves. A flag
someone can turn on is no way to roll something out to a few people at a time.
-}
module Auth.Feature (
  Feature,
  apiKeys,
  allFeatures,
  parseFeatures,
  renderFeatures,
  hasFeature,
) where

import Data.Text qualified as T
import Relude

-- | A feature an account may be let into. Space-separated on the row, as scopes are.
type Feature = Text

{- | Programmatic access: minting API keys, and through them the remote MCP
server. Gated because a key is a credential that outlives the session that made
it, and because every write it can make lands in the real library.
-}
apiKeys :: Feature
apiKeys = "apiKeys"

{- | Every feature that can be granted.

A name outside this list is dropped on the way in and ignored on the way out, so
a feature that has gone general -- or been renamed -- cannot go on quietly
granting itself to whichever accounts happen to still name it.
-}
allFeatures :: [Feature]
allFeatures = [apiKeys]

parseFeatures :: Text -> [Feature]
parseFeatures = filter (`elem` allFeatures) . T.words

renderFeatures :: [Feature] -> Text
renderFeatures = T.unwords . ordNub . filter (`elem` allFeatures)

-- | Whether a @users.features@ column grants a feature.
hasFeature :: Feature -> Text -> Bool
hasFeature feature = elem feature . parseFeatures
