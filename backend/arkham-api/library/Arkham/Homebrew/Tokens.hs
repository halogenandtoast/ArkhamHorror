module Arkham.Homebrew.Tokens (
  module Arkham.Homebrew.TokenDefs,
  customTokenDefs,
  customTokenRevealEffect,
) where

import Arkham.ChaosToken.Types
import Arkham.Homebrew.CircusExMortis.Tokens qualified as CircusExMortis
import Arkham.Homebrew.TokenDefs
import Arkham.Prelude

customTokenDefs :: Map Text CustomTokenDef
customTokenDefs =
  mapFromList
    [ (tokenSlug def, def)
    | def <- CircusExMortis.customTokens
    ]

-- | Engine-level reveal behavior for a token face; 'RevealNoEffect' for
-- official faces and unregistered custom tokens.
customTokenRevealEffect :: ChaosTokenFace -> CustomTokenReveal
customTokenRevealEffect (CustomToken slug) =
  maybe RevealNoEffect tokenRevealEffect (lookup slug customTokenDefs)
customTokenRevealEffect _ = RevealNoEffect
