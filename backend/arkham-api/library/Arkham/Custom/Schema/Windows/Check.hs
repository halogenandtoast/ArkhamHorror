{-# LANGUAGE TemplateHaskell #-}

{- | Compile-time validation of "Arkham.Custom.Schema.Windows.Table".

Separate from the module that splices it because Template Haskell will not let a
splice use a function bound in its own module.
-}
module Arkham.Custom.Schema.Windows.Check (checkedTable) where

import Arkham.Custom.Schema.Windows.Table (windowTable)
import Arkham.Matcher (WindowMatcher)
import Arkham.Prelude
import Arkham.Window (WindowType)
import Control.Monad.Fail (fail)
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Haskell.TH (Exp, Q, reify, runIO)
import Language.Haskell.TH.Syntax (Con (..), Dec (..), Info (..), Name, nameBase)
import Language.Haskell.TH.Syntax qualified as TH

constructorNames :: Name -> Q (Set Text)
constructorNames name =
  reify name >>= \case
    TyConI (DataD _ _ _ _ dataCons _) -> pure $ Set.fromList (concatMap conNames dataCons)
    TyConI (NewtypeD _ _ _ _ con _) -> pure $ Set.fromList (conNames con)
    other -> fail $ "Custom.Schema.Windows: " <> nameBase name <> " is not a data type: " <> show other
 where
  conNames = \case
    NormalC n _ -> [T.pack (nameBase n)]
    RecC n _ -> [T.pack (nameBase n)]
    InfixC _ n _ -> [T.pack (nameBase n)]
    ForallC _ _ c -> conNames c
    GadtC ns _ _ -> map (T.pack . nameBase) ns
    RecGadtC ns _ _ -> map (T.pack . nameBase) ns

checkedTable :: Q Exp
checkedTable = do
  matchers <- constructorNames ''WindowMatcher
  windows <- constructorNames ''WindowType
  let
    tableKeys = Set.fromList (map fst windowTable)
    named = Set.fromList (concatMap snd windowTable)
    unknownMatchers = tableKeys `Set.difference` matchers
    unknownWindows = named `Set.difference` windows
    missing = matchers `Set.difference` tableKeys
    complain what names =
      [what <> ": " <> T.unpack (T.intercalate ", " (Set.toList names)) | not (Set.null names)]
    problems =
      complain "not WindowMatcher constructors" unknownMatchers
        <> complain "not WindowType constructors" unknownWindows
        <> complain "WindowMatcher constructors with no entry in windowTable" missing
  unless (null problems)
    $ fail
    $ "Arkham.Custom.Schema.Windows.Table is out of step with the engine.\n  "
    <> intercalate "\n  " problems
    <> "\n\nEvery WindowMatcher needs an entry saying which window it fires on;\n\
       \use an empty list for one that fires on no window of its own."
  -- Coverage is worth seeing when the table is rebuilt, since an entry can be
  -- present and still be an empty list.
  runIO
    $ putStrLn
    $ "Custom card window table: "
    <> tshow (length (filter (notNull . snd) windowTable))
    <> " of "
    <> tshow (Set.size matchers)
    <> " matchers mapped to a window."
  TH.lift windowTable
