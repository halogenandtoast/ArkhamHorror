{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Token where

import Arkham.Prelude hiding (Index)
import Control.Monad.Fail (fail)
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)
import Data.Text qualified as T
import GHC.OverloadedLabels

-- Make sure to update src/arkham/types/Token.ts
data Token
  = Aether
  | AlarmLevel
  | Ammo
  | Antiquity
  | Bounty
  | Brilliance
  | Chance
  | Charge
  | Civilian
  | Clue
  | Corruption
  | Damage
  | DarknessLevel
  | Depletion
  | Depth
  | Discovery
  | Doom
  | Dose
  | Durability
  | Eclipse
  | Empowerment
  | Evidence
  | Growth
  | Horror
  | Inspiration
  | Key
  | Kindling
  | Lead
  | Leyline
  | Lock
  | LostSoul
  | Memory
  | Mutation
  | Newspaper
  | Obligation
  | Obsession
  | Offering
  | Overgrowth
  | Pillar
  | Portent
  | Redemption
  | Renown
  | Resource
  | Rumor
  | ScoutingReport
  | Scrap
  | Seal
  | Secret
  | Seed
  | Shard
  | Shell
  | Shipment
  | Sign
  | Snare
  | Study
  | Supply
  | Suspicion
  | Switch
  | Target
  | Ticket
  | Time
  | TimeCapsule
  | Truth
  | Try
  | Ward
  | Warning
  | Whistle
  | Wish
  | -- | A marker a custom card set brings with it, named by the slug its set
    -- declared (@Arkham.Custom.SetDefs@). Open rather than a constructor per
    -- set: the engine never knows what a set will call its tokens, and a token
    -- like Tension has no game effect of its own beyond being counted.
    Custom Text
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSONKey, FromJSONKey)

{- | What marks a token as a set's own rather than one the engine prints.

Prefixed rather than written bare: a set is free to call its token anything, and
a slug that happened to match a token the engine gained later would silently
become that token in every saved game. The prefix is the one thing a core name
can never be.
-}
customTokenPrefix :: Text
customTokenPrefix = "custom:"

{- | Core tokens serialize as their bare constructor name, as the derived
all-nullary encoding did, so saved games round-trip untouched. A 'Custom' token
serializes as its prefixed slug, which no core name can be.
-}
instance ToJSON Token where
  toJSON = \case
    Custom slug -> toJSON (customTokenPrefix <> slug)
    t -> toJSON (tshow t)

instance FromJSON Token where
  parseJSON = withText "Token" \t -> case T.stripPrefix customTokenPrefix t of
    Just slug | not (T.null slug) -> pure $ Custom slug
    _ -> case lookup t coreTokenByName of
      Just token -> pure token
      Nothing -> fail $ "Unknown token: " <> show t

-- | Every token the engine itself prints.
coreTokens :: [Token]
coreTokens =
  [ fromConstr con
  | con <- dataTypeConstrs (dataTypeOf (Custom ""))
  , showConstr con /= "Custom"
  ]

coreTokenByName :: Map Text Token
coreTokenByName = mapFromList [(tshow t, t) | t <- coreTokens]

{- | The slug a custom token was declared under, for whatever has to look its
name and image up in the set that brought it.
-}
customTokenSlugOf :: Token -> Maybe Text
customTokenSlugOf = \case
  Custom slug -> Just slug
  _ -> Nothing

tokenIsUse :: Token -> Bool
tokenIsUse = \case
  Damage -> False
  Horror -> False
  Clue -> False
  Doom -> False
  -- A set's own token is a marker its cards place on themselves, not a supply
  -- anything spends, so it is never a use.
  Custom _ -> False
  _ -> True

instance IsLabel "damage" Token where
  fromLabel = Damage

instance IsLabel "horror" Token where
  fromLabel = Horror

instance IsLabel "resource" Token where
  fromLabel = Resource

instance IsLabel "supply" Token where
  fromLabel = Supply

instance IsLabel "ammo" Token where
  fromLabel = Ammo

instance IsLabel "charge" Token where
  fromLabel = Charge

instance IsLabel "secret" Token where
  fromLabel = Secret

instance IsLabel "obligation" Token where
  fromLabel = Obligation

instance IsLabel "discovery" Token where
  fromLabel = Discovery

instance IsLabel "dose" Token where
  fromLabel = Dose

instance IsLabel "chance" Token where
  fromLabel = Chance

instance IsLabel "obsession" Token where
  fromLabel = Obsession

instance IsLabel "offering" Token where
  fromLabel = Offering

instance IsLabel "sign" Token where
  fromLabel = Sign

instance IsLabel "clue" Token where
  fromLabel = Clue

instance IsLabel "doom" Token where
  fromLabel = Doom

instance IsLabel "antiquity" Token where
  fromLabel = Antiquity

instance IsLabel "portent" Token where
  fromLabel = Portent

instance IsLabel "renown" Token where
  fromLabel = Renown

instance IsLabel "rumor" Token where
  fromLabel = Rumor

instance IsLabel "warning" Token where
  fromLabel = Warning

instance IsLabel "truth" Token where
  fromLabel = Truth

type Tokens = Map Token Int

flipClues :: Int -> Tokens -> Tokens
flipClues n tokens =
  let clueVal = findWithDefault 0 Clue tokens
      n' = min n clueVal
   in if clueVal == 0 then tokens else addTokens Doom n' (subtractTokens Clue n' tokens)

flipDoom :: Int -> Tokens -> Tokens
flipDoom n tokens =
  let doomVal = findWithDefault 0 Doom tokens
      n' = min n doomVal
   in if doomVal == 0 then tokens else addTokens Clue n' (subtractTokens Doom n' tokens)

hasToken :: Token -> Tokens -> Bool
hasToken token tokens = countTokens token tokens > 0

countTokens :: Token -> Tokens -> Int
countTokens token tokens = findWithDefault 0 token tokens

addTokens :: Token -> Int -> Tokens -> Tokens
addTokens token amount tokens =
  let val = max 0 (findWithDefault 0 token tokens + amount)
   in if val == 0
        then deleteMap token tokens
        else insertMap token val tokens

removeAllTokens :: Token -> Tokens -> Tokens
removeAllTokens token tokens = deleteMap token tokens
decrementTokens :: Token -> Tokens -> Tokens
decrementTokens token = decrementTokensBy token 1

decrementTokensBy :: Token -> Int -> Tokens -> Tokens
decrementTokensBy token n = subtractTokens token n

subtractTokens :: Token -> Int -> Tokens -> Tokens
subtractTokens token amount = addTokens token (-amount)

setTokens :: Token -> Int -> Tokens -> Tokens
setTokens token (max 0 -> amount) tokens
  | amount > 0 = insertMap token amount tokens
  | otherwise = deleteMap token tokens
