{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.ChaosToken where

import Arkham.ChaosToken.Types
import Arkham.ChaosToken.Types qualified as ChaosToken
import {-# SOURCE #-} Arkham.Matcher.Asset
import {-# SOURCE #-} Arkham.Matcher.Enemy
import Arkham.Prelude
import Data.Aeson.TH
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
  | RevealedChaosTokens ChaosTokenMatcher
  | ChaosTokenMatchesOrElse ChaosTokenMatcher ChaosTokenMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Not ChaosTokenMatcher where
  not_ = NotChaosToken

instance IsLabel "any" ChaosTokenMatcher where
  fromLabel = AnyChaosToken

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
