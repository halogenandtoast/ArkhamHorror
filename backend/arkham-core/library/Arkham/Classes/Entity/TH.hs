{-# LANGUAGE TemplateHaskell #-}

module Arkham.Classes.Entity.TH (
  module X,
  module Arkham.Classes.Entity.TH,
) where

import Arkham.Prelude

import Arkham.Classes.Entity as X
import Data.Char qualified as C
import Language.Haskell.TH.Syntax hiding (Name)
import Language.Haskell.TH.Syntax qualified as TH

buildEntity :: String -> Q [Dec]
buildEntity nm = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nm) [VarT (mkName "a")]
  let conz = mapMaybe extractCon instances
  pure
    [ DataD
        []
        (TH.mkName nm)
        []
        Nothing
        conz
        [DerivClause (Just StockStrategy) (map ConT [''Show, ''Eq])]
    ]
 where
  extractCon (InstanceD _ _ (AppT _ con@(ConT name)) _) =
    Just
      $ NormalC
        (TH.mkName $ nameBase name ++ "'")
        [(Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, con)]
  extractCon _ = Nothing

buildEntityLookupList :: String -> Q Exp
buildEntityLookupList nm = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nm) [VarT (mkName "a")]
  let conz = mapMaybe extractCon instances
  pure $ ListE conz
 where
  extractCon (InstanceD _ _ (AppT _ (ConT name)) _) =
    Just
      $ AppE
        (AppE (VarE 'fmap) (ConE $ TH.mkName $ nameBase name ++ "'"))
        (VarE $ toFunName $ nameBase name)
  extractCon _ = Nothing
  toFunName [] = TH.mkName ""
  toFunName (x : xs) = TH.mkName $ C.toLower x : xs

buildEntityLookupList2 :: TH.Name -> Q Exp
buildEntityLookupList2 nm = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nameBase nm) [VarT (mkName "a")]
  let conz = mapMaybe extractCon instances
  pure $ ListE conz
 where
  extractCon (InstanceD _ _ (AppT _ (ConT name)) _) =
    Just
      $ AppE
        (AppE (VarE 'fmap) (ConE nm))
        (VarE $ toFunName $ nameBase name)
  extractCon _ = Nothing
  toFunName [] = TH.mkName ""
  toFunName (x : xs) = TH.mkName $ C.toLower x : xs

entityRunMessage :: String -> Q Exp
entityRunMessage nm = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nm) [VarT (mkName "a")]
  a <- newName "a"
  msg <- newName "msg"
  x <- newName "x"
  let matches = mapMaybe (toMatch msg x) instances
  pure $ LamE [VarP msg, VarP a] $ CaseE (VarE a) matches
 where
  toMatch msg x (InstanceD _ _ (AppT _ (ConT name)) _) =
    Just
      $ Match
        (ConP (TH.mkName $ nameBase name <> "'") [] [VarP x])
        ( NormalB
            $ AppE
              (AppE (VarE 'fmap) (ConE $ TH.mkName $ nameBase name ++ "'"))
              (AppE (AppE (VarE $ TH.mkName "runMessage") (VarE msg)) (VarE x))
        )
        []
  toMatch _ _ _ = Nothing

entityF :: String -> TH.Name -> Q Exp
entityF nm f = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nm) [VarT (mkName "a")]
  a <- newName "a"
  x <- newName "x"
  let matches = mapMaybe (toMatch f x) instances
  pure $ LamE [VarP a] $ CaseE (VarE a) matches
 where
  toMatch g x (InstanceD _ _ (AppT _ (ConT name)) _) =
    Just
      $ Match
        (ConP (TH.mkName $ nameBase name <> "'") [] [VarP x])
        (NormalB $ AppE (VarE g) (VarE x))
        []
  toMatch _ _ _ = Nothing

entityF2 :: String -> TH.Name -> Q Exp
entityF2 nm f = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nm) [VarT (mkName "a")]
  a <- newName "a"
  p1 <- newName "p1"
  p2 <- newName "p2"
  x <- newName "x"
  let matches = mapMaybe (toMatch f p1 p2 x) instances
  pure $ LamE [VarP p1, VarP p2, VarP a] $ CaseE (VarE a) matches
 where
  toMatch g p1 p2 x (InstanceD _ _ (AppT _ (ConT name)) _) =
    Just
      $ Match
        (ConP (TH.mkName $ nameBase name <> "'") [] [VarP x])
        (NormalB $ AppE (AppE (AppE (VarE g) (VarE p1)) (VarE p2)) (VarE x))
        []
  toMatch _ _ _ _ _ = Nothing

entityF1 :: String -> TH.Name -> Q Exp
entityF1 nm f = do
  instances <- reifyInstances (TH.mkName $ "Is" ++ nm) [VarT (mkName "a")]
  a <- newName "a"
  p1 <- newName "p1"
  x <- newName "x"
  let matches = mapMaybe (toMatch f p1 x) instances
  pure $ LamE [VarP p1, VarP a] $ CaseE (VarE a) matches
 where
  toMatch g p1 x (InstanceD _ _ (AppT _ (ConT name)) _) =
    Just
      $ Match
        (ConP (TH.mkName $ nameBase name <> "'") [] [VarP x])
        (NormalB $ AppE (AppE (VarE g) (VarE p1)) (VarE x))
        []
  toMatch _ _ _ _ = Nothing
