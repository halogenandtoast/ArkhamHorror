{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.ChaosToken (module Arkham.ChaosToken, module Arkham.ChaosToken.Types) where

import Arkham.Calculation
import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Helpers.Calculation
import Arkham.Prelude

pattern PositiveModifier :: Int -> ChaosTokenModifier
pattern PositiveModifier n <- CalculatedModifier (Fixed n)
  where
    PositiveModifier n = CalculatedModifier (Fixed n)

pattern ZeroModifier :: ChaosTokenModifier
pattern ZeroModifier <- CalculatedModifier (Fixed 0)
  where
    ZeroModifier = CalculatedModifier (Fixed 0)

pattern NegativeModifier :: Int -> ChaosTokenModifier
pattern NegativeModifier n <- CalculatedModifier (Negated (Fixed n))
  where
    NegativeModifier n = CalculatedModifier (Negated (Fixed n))

pattern DoubleNegativeModifier :: Int -> ChaosTokenModifier
pattern DoubleNegativeModifier n <- CalculatedModifier (Negated (MultiplyCalculation (Fixed 2) (Fixed n)))
  where
    DoubleNegativeModifier n = CalculatedModifier (Negated (MultiplyCalculation (Fixed 2) (Fixed n)))

chaosTokenValue :: HasGame m => ChaosTokenValue -> m (Maybe Int)
chaosTokenValue (ChaosTokenValue _ modifier) = chaosTokenModifierToInt modifier

chaosTokenModifierToInt :: HasGame m => ChaosTokenModifier -> m (Maybe Int)
chaosTokenModifierToInt = \case
  CalculatedModifier n -> Just <$> calculate n
  AutoSuccessModifier -> pure Nothing
  AutoFailModifier -> pure Nothing
  NoModifier -> pure $ Just 0

-- TODO: this is a huge bandaid and might not work later
instance Semigroup ChaosTokenModifier where
  -- If a skill test both automatically succeeds and automatically fails, [for
  -- instance by drawing {Autofail} and {ElderSign} with Olive McBride while
  -- playing as Father Mateo], the automatic failure takes precedence, and the
  -- test automatically fails.
  AutoFailModifier <> _ = AutoFailModifier
  _ <> AutoFailModifier = AutoFailModifier
  AutoSuccessModifier <> _ = AutoSuccessModifier
  _ <> AutoSuccessModifier = AutoSuccessModifier
  NoModifier <> a = a
  a <> NoModifier = a
  CalculatedModifier a <> CalculatedModifier b =
    CalculatedModifier (SumCalculation [a, b])

instance Monoid ChaosTokenModifier where
  mempty = NoModifier
