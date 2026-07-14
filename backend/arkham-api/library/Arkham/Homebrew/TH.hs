{-# LANGUAGE TemplateHaskell #-}

module Arkham.Homebrew.TH (
  discoverInstances,
  declareOpenExtension,
  declareHomebrewTraits,
  declareHomebrewScenarioDeckKeys,
  declareHomebrewActions,
) where

import Arkham.Action (Action (HomebrewAction))
import Arkham.Prelude
import Arkham.Scenario.Deck (ScenarioDeckKey (HomebrewScenarioDeckKey))
import Arkham.Trait (Trait (HomebrewTrait))
import Control.Monad.Fail
import Data.List qualified as List
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

{- | Declare homebrew extension values for a sum type that carries an open
@Text@-tagged escape-hatch constructor (e.g. @HomebrewTrait Text@ on 'Trait',
@HomebrewAction Text@ on 'Arkham.Action.Action'). For each name @N@ it emits a
bidirectional pattern synonym @pattern N = <Ctor> "N"@ — so a campaign's module
is just the list of names, and card code references a named, typo-checked value
exactly as it would a core constructor. When @mListName@ is @Just name@, it also
emits @name :: [Ty]@ containing all of them (used e.g. to enumerate the full
trait universe).

Guarantees enforced at compile time:

  * __No duplication with core.__ If any name already exists as a real
    constructor of @Ty@, compilation fails. So when a homebrew value is later
    promoted into the core enum, the clash is caught here and the name must be
    removed from the homebrew list.

  * __Promotion stays parse-safe.__ The generated tag equals the name, so a
    @<Ctor> "N"@ serializes identically to (or, for tagged encodings, is
    recoverable to) a core constructor @N@. The core type's FromJSON resolves
    core names first, so existing @"N"@ data decodes straight to the promoted
    constructor — no migration, no stale escape-hatch values.

  * __No self-duplication.__ A repeated name fails rather than emitting a
    clashing pattern synonym.

Generated declarations reference @Ty@ and its constructor by resolved name, so
the splice site needs no import of the core module — only @TemplateHaskell@ (and
the default @PatternSynonyms@/@OverloadedStrings@).
-}
declareOpenExtension
  :: Name
  -- ^ the sum type, for @reify@ and signatures (e.g. @''Trait@)
  -> Name
  -- ^ its open escape-hatch constructor (e.g. @'HomebrewTrait@)
  -> Maybe String
  -- ^ @Just name@ to also emit @name :: [Ty]@ of every value
  -> [String]
  -- ^ the names (each is both the pattern-synonym name and its @Text@ tag)
  -> Q [Dec]
declareOpenExtension tyName ctorName mListName names = do
  let what = nameBase ctorName
  case names List.\\ List.nub names of
    [] -> pure ()
    dups -> fail $ what <> ": duplicate name(s): " <> List.intercalate ", " (List.nub dups)
  coreNames <-
    reify tyName >>= \case
      TyConI (DataD _ _ _ _ cons' _) -> pure [nameBase n | c <- cons', n <- conNames c]
      _ -> fail $ nameBase tyName <> " is not a data declaration"
  case filter (`elem` coreNames) names of
    [] -> pure ()
    clashes ->
      fail
        $ what
        <> ": name(s) already exist as core "
        <> nameBase tyName
        <> " constructors; they have been promoted into core — remove them from this homebrew list: "
        <> List.intercalate ", " clashes
  let
    synonyms s =
      let n = mkName s
       in [ PatSynSigD n (ConT tyName)
          , PatSynD n (PrefixPatSyn []) ImplBidir (ConP ctorName [] [LitP (StringL s)])
          ]
    listDecs name =
      let ln = mkName name
       in [ SigD ln (AppT ListT (ConT tyName))
          , ValD (VarP ln) (NormalB (ListE [AppE (ConE ctorName) (LitE (StringL s)) | s <- names])) []
          ]
  pure (concatMap synonyms names <> maybe [] listDecs mListName)

-- | @$(declareHomebrewTraits ["AI", ...])@ — pattern synonyms over
-- 'Arkham.Trait.HomebrewTrait' plus a @traits :: [Trait]@ aggregate.
declareHomebrewTraits :: [String] -> Q [Dec]
declareHomebrewTraits = declareOpenExtension ''Trait 'HomebrewTrait (Just "traits")

-- | @$(declareHomebrewScenarioDeckKeys ["ScanningDeck", ...])@ — pattern
-- synonyms over 'Arkham.Scenario.Deck.HomebrewScenarioDeckKey'.
declareHomebrewScenarioDeckKeys :: [String] -> Q [Dec]
declareHomebrewScenarioDeckKeys = declareOpenExtension ''ScenarioDeckKey 'HomebrewScenarioDeckKey Nothing

-- | @$(declareHomebrewActions ["Scan", ...])@ — pattern synonyms over
-- 'Arkham.Action.HomebrewAction' plus an @actions :: [Action]@ aggregate, which
-- a campaign's @Defs.hs@ folds into @Arkham.Homebrew.Defs.allActions@.
declareHomebrewActions :: [String] -> Q [Dec]
declareHomebrewActions = declareOpenExtension ''Action 'HomebrewAction (Just "actions")

conNames :: Con -> [Name]
conNames = \case
  NormalC n _ -> [n]
  RecC n _ -> [n]
  InfixC _ n _ -> [n]
  ForallC _ _ c -> conNames c
  GadtC ns _ _ -> ns
  RecGadtC ns _ _ -> ns
