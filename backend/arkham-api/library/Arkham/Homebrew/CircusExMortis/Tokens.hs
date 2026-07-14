module Arkham.Homebrew.CircusExMortis.Tokens where

import Arkham.ChaosToken.Types
import Arkham.Homebrew.TokenDefs

-- | The moon (☾) token (guide p1): "0. Seal this token on your investigator
-- card and reveal another token." No effect revealed outside a skill test.
pattern MoonToken :: ChaosTokenFace
pattern MoonToken = CustomToken ":circus-ex-mortis:moon"

customTokens :: [CustomTokenDef]
customTokens =
  [ CustomTokenDef
      { tokenSlug = ":circus-ex-mortis:moon"
      , tokenRevealEffect = SealOnRevealerAndRevealAnother
      }
  ]
