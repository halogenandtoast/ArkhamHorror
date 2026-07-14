{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TH (discoverInstances) where

import Arkham.Prelude
import Language.Haskell.TH

{- | @$(discoverInstances ''SomeClass 'someMethod)@ expands to
@mconcat [someMethod \@T1, someMethod \@T2, ...]@ for every instance of the
class visible in the enclosing module (bring them into scope with an
instance-only discovery aggregator; see the @cards-discover --instances@
mode). The method must be callable via a visible type application
(@AllowAmbiguousTypes@ on the class).
-}
discoverInstances :: Name -> Name -> Q Exp
discoverInstances cls method = do
  ClassI _ instances <- reify cls
  let
    instanceHead (InstanceD _ _ (AppT _ ty) _) = Just ty
    instanceHead _ = Nothing
    tys = mapMaybe instanceHead instances
  appE (varE 'mconcat) (listE [varE method `appTypeE` pure ty | ty <- tys])
