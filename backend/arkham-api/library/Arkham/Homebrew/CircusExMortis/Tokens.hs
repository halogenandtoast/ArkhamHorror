module Arkham.Homebrew.CircusExMortis.Tokens where

import Arkham.ChaosToken.Types
import Arkham.Homebrew.TokenDefs
import Arkham.Prelude

{- | The moon (☾) token (guide p1): "0. Seal this token on your investigator
card and reveal another token." No effect revealed outside a skill test.
-}
pattern MoonToken :: ChaosTokenFace
pattern MoonToken = CustomToken ":circus-ex-mortis:moon"

data CircusExMortisTokens

instance IsHomebrewTokens CircusExMortisTokens where
  homebrewTokens =
    [ CustomTokenDef
        { tokenSlug = ":circus-ex-mortis:moon"
        , tokenRevealEffect = SealOnRevealerAndRevealAnother
        , tokenPool = Nothing
        }
    ]
