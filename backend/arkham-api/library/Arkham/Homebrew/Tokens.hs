{-# LANGUAGE TemplateHaskell #-}

{- | Custom chaos tokens contributed by homebrew campaigns. Campaigns are
discovered: any @Arkham/Homebrew/<Name>/Tokens.hs@ with an 'IsHomebrewTokens'
instance is folded in automatically — no edits here when adding a campaign.
-}
module Arkham.Homebrew.Tokens (
  module Arkham.Homebrew.TokenDefs,
  customTokenDefs,
  customTokenRevealEffect,
  chaosTokenFacePool,
  pooledChaosTokenFaces,
) where

import Arkham.ChaosToken.Types
import Arkham.Homebrew.TH
import Arkham.Homebrew.TokenDefs
import Arkham.Homebrew.TokenEntries ()
import Arkham.Prelude

customTokenDefs :: Map Text CustomTokenDef
customTokenDefs =
  mapFromList
    [ (tokenSlug def, def)
    | def <- $(discoverInstances ''IsHomebrewTokens 'homebrewTokens)
    ]

-- | Engine-level reveal behavior for a token face; 'RevealNoEffect' for
-- official faces and unregistered custom tokens.
customTokenRevealEffect :: ChaosTokenFace -> CustomTokenReveal
customTokenRevealEffect (CustomToken slug) =
  maybe RevealNoEffect tokenRevealEffect (lookup slug customTokenDefs)
customTokenRevealEffect _ = RevealNoEffect

{- | The size of the physical supply a face is drawn from, or 'Nothing' when the
face is minted on demand. Official pooled faces are fixed by the rules; homebrew
faces declare their own supply on their 'CustomTokenDef'.
-}
chaosTokenFacePool :: ChaosTokenFace -> Maybe Int
chaosTokenFacePool = \case
  BlessToken -> Just 10
  CurseToken -> Just 10
  FrostToken -> Just 8
  BloodToken -> Just 12
  CustomToken slug -> tokenPool =<< lookup slug customTokenDefs
  _ -> Nothing

-- | Every face that has a physical supply, paired with its size.
pooledChaosTokenFaces :: [(ChaosTokenFace, Int)]
pooledChaosTokenFaces =
  [ (face, n)
  | face <- [BlessToken, CurseToken, FrostToken, BloodToken] <> map (CustomToken . tokenSlug) (toList customTokenDefs)
  , Just n <- [chaosTokenFacePool face]
  ]
