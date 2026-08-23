{-# LANGUAGE AllowAmbiguousTypes #-}

module Arkham.Homebrew.TokenDefs where

import Arkham.Prelude

-- | What the engine does when a custom token is revealed during a skill test.
-- (A custom token revealed outside a skill test never has an engine effect,
-- matching official symbol-token behavior; scenarios can add their own.)
data CustomTokenReveal
  = -- | No engine-level effect; the scenario's 'HasChaosTokenValue' and
    -- message handlers define everything.
    RevealNoEffect
  | -- | Reveal another token after this one (bless\/curse\/frost style).
    RevealAnother
  | -- | Seal this token on the revealing investigator's investigator card and
    -- reveal another token (Circus Ex Mortis moon style).
    SealOnRevealerAndRevealAnother
  deriving stock (Show, Eq)

data CustomTokenDef = CustomTokenDef
  { tokenSlug :: Text
  -- ^ e.g. @":circus-ex-mortis:moon"@; the segment after the last colon is
  -- the display key (format tag, label, frontend @ct_<key>.png@ icon).
  , tokenRevealEffect :: CustomTokenReveal
  , tokenPool :: Maybe Int
  -- ^ How many physical tokens of this face exist, when the face is drawn from
  -- a limited supply like bless\/curse\/frost\/blood. 'Nothing' means the face
  -- is minted on demand and can always be added to the chaos bag.
  }

-- | Implement in your campaign's @Tokens.hs@ on a campaign-local tag type;
-- the instance is discovered automatically (see 'Arkham.Homebrew.Tokens').
class IsHomebrewTokens a where
  homebrewTokens :: [CustomTokenDef]
