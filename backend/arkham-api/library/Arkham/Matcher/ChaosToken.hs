{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.ChaosToken where

import Arkham.ChaosToken.Types
import Arkham.ChaosToken.Types qualified as ChaosToken
import {-# SOURCE #-} Arkham.Matcher.Asset
import {-# SOURCE #-} Arkham.Matcher.Enemy
import {-# SOURCE #-} Arkham.Matcher.Investigator
import Arkham.Prelude
import Data.Aeson.TH
import Data.Text qualified as T
import GHC.OverloadedLabels

data ChaosTokenMatcher
  = WithNegativeModifier
  | ChaosTokenFaceIs ChaosTokenFace
  | ChaosTokenFaceIsNot ChaosTokenFace
  | ChaosTokenMatchesAny [ChaosTokenMatcher]
  | AnyChaosToken
  | IsSymbol
  | InTokenPool ChaosTokenMatcher
  | ChaosTokenMatches [ChaosTokenMatcher]
  | IncludeSealed ChaosTokenMatcher
  | IncludeTokenPool ChaosTokenMatcher
  | WouldReduceYourSkillValueToZero
  | IsInfestationToken ChaosTokenMatcher
  | NotChaosToken ChaosTokenMatcher
  | SealedOnAsset AssetMatcher ChaosTokenMatcher
  | SealedOnEnemy EnemyMatcher ChaosTokenMatcher
  | SealedOnInvestigator InvestigatorMatcher ChaosTokenMatcher
  | RevealedChaosTokens ChaosTokenMatcher
  | ChaosTokenRevealedBy InvestigatorMatcher
  | ChaosTokenMatchesOrElse ChaosTokenMatcher ChaosTokenMatcher
  | ChaosTokenIs ChaosTokenId
  deriving stock (Show, Eq, Ord, Data)

chaosTokenIs :: ChaosToken -> ChaosTokenMatcher
chaosTokenIs = ChaosTokenIs . chaosTokenId

pattern RevealedChaosToken :: ChaosTokenMatcher
pattern RevealedChaosToken = RevealedChaosTokens AnyChaosToken

instance ToDisplay ChaosTokenMatcher where
  toDisplay = \case
    ChaosTokenIs _ -> "Specific chaos token"
    WithNegativeModifier -> "Chaos token with negative modifier"
    ChaosTokenFaceIs face -> toDisplay face
    ChaosTokenFaceIsNot face -> "not " <> toDisplay face
    ChaosTokenMatchesAny inner -> "one of: " <> T.intercalate "," (map toDisplay inner)
    AnyChaosToken -> "any chaos token"
    IsSymbol -> "symbol chaos token"
    InTokenPool inner -> toDisplay inner <> " in the token pool"
    ChaosTokenMatches inner -> toSentence $ map toDisplay inner
    IncludeSealed inner -> toDisplay inner <> " in all play areas"
    IncludeTokenPool inner -> toDisplay inner
    WouldReduceYourSkillValueToZero -> "chaos token that would reduce you skill value to zero"
    IsInfestationToken inner -> toDisplay inner
    NotChaosToken inner -> "not " <> toDisplay inner
    SealedOnAsset _ inner -> toDisplay inner <> " sealed on a relevant asset"
    SealedOnEnemy _ inner -> toDisplay inner <> " sealed on a relevant enemy"
    SealedOnInvestigator _ inner -> toDisplay inner <> " sealed on a relevant enemy"
    RevealedChaosTokens inner -> toDisplay inner <> " among revealed"
    ChaosTokenMatchesOrElse inner1 inner2 -> toDisplay inner1 <> " if possible, otherwise " <> toDisplay inner2
    ChaosTokenRevealedBy _ -> "Revealed chaos token"

instance Not ChaosTokenMatcher where
  not_ = NotChaosToken

instance IsLabel "any" ChaosTokenMatcher where
  fromLabel = AnyChaosToken

instance IsLabel "frost" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs FrostToken

instance IsLabel "skull" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Skull

instance IsLabel "cultist" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs ChaosToken.Cultist

instance IsLabel "tablet" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Tablet

instance IsLabel "elderthing" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs ElderThing

instance IsLabel "eldersign" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs ElderSign

instance IsLabel "autofail" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs AutoFail

instance IsLabel "bless" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs BlessToken

instance IsLabel "curse" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs CurseToken

instance IsLabel "plusone" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs PlusOne

instance IsLabel "zero" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Zero

instance IsLabel "0" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Zero

instance IsLabel "+1" ChaosTokenMatcher where
  fromLabel = ChaosTokenFaceIs Zero

instance Semigroup ChaosTokenMatcher where
  AnyChaosToken <> x = x
  x <> AnyChaosToken = x
  ChaosTokenMatches xs <> ChaosTokenMatches ys = ChaosTokenMatches $ xs <> ys
  ChaosTokenMatches xs <> x = ChaosTokenMatches $ xs <> [x]
  x <> ChaosTokenMatches xs = ChaosTokenMatches $ x : xs
  x <> y = ChaosTokenMatches [x, y]

instance Monoid ChaosTokenMatcher where
  mempty = AnyChaosToken

$(deriveJSON defaultOptions ''ChaosTokenMatcher)
