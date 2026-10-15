{-# LANGUAGE TemplateHaskellQuotes #-}

module Arkham.Homebrew.TH (
  discoverInstances,
  discoveredModules,
  declareOpenExtension,
  declareOpenExtensionWith,
  declareHomebrewTraits,
  declareHomebrewScenarioDeckKeys,
  declareHomebrewActions,
  declareHomebrewConcealedCardKinds,
) where

import Arkham.Action (Action (HomebrewAction))
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind (
  ConcealedCardKind (HomebrewConcealedCardKind),
 )
import Arkham.Prelude
import Arkham.Scenario.Deck (ScenarioDeckKey (HomebrewScenarioDeckKey))
import Arkham.Trait (Trait (HomebrewTrait))
import Control.Monad.Fail
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax qualified as THS

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

{- | @discoveredModules \@DiscoveredModules@ — the module list that
@cards-discover --instances@ put in the aggregator's entries module.

Nothing reads the result. It exists because GHC's recompilation checker cannot
see the instances 'discoverInstances' finds by 'reify': a module spliced over
them looks unchanged when the entries module merely gains an import, so the
aggregate silently keeps the campaigns it was last compiled with. Referencing
the marker ties this module's recompilation to that list.
-}
discoveredModules :: forall s. KnownSymbol s => Text
discoveredModules = pack (symbolVal (Proxy @s))

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
declareOpenExtension = declareOpenExtensionWith id

{- | 'declareOpenExtension' with a tag that differs from the declared name. Only
for a type whose tag has to carry more than the name -- a concealed card kind
names the box its mini-card art belongs to -- which gives up the
promotion-stays-parse-safe guarantee above; the clash check against core
constructors still holds.
-}
declareOpenExtensionWith
  :: (String -> String) -> Name -> Name -> Maybe String -> [String] -> Q [Dec]
declareOpenExtensionWith tagFor tyName ctorName mListName names = do
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
          , PatSynD n (PrefixPatSyn []) ImplBidir (ConP ctorName [] [LitP (StringL (tagFor s))])
          ]
    listDecs name =
      let ln = mkName name
       in [ SigD ln (AppT ListT (ConT tyName))
          , ValD (VarP ln) (NormalB (ListE [AppE (ConE ctorName) (LitE (StringL (tagFor s))) | s <- names])) []
          ]
  pure (concatMap synonyms names <> maybe [] listDecs mListName)

{- | @$(declareHomebrewTraits ["AI", ...])@ — pattern synonyms over
'Arkham.Trait.HomebrewTrait' plus a @traits :: [Trait]@ aggregate.
-}
declareHomebrewTraits :: [String] -> Q [Dec]
declareHomebrewTraits = declareOpenExtension ''Trait 'HomebrewTrait (Just "traits")

{- | @$(declareHomebrewScenarioDeckKeys ["ScanningDeck", ...])@ — pattern
synonyms over 'Arkham.Scenario.Deck.HomebrewScenarioDeckKey'.
-}
declareHomebrewScenarioDeckKeys :: [String] -> Q [Dec]
declareHomebrewScenarioDeckKeys = declareOpenExtension ''ScenarioDeckKey 'HomebrewScenarioDeckKey Nothing

{- | @$(declareHomebrewActions ["Scan", ...])@ — pattern synonyms over
'Arkham.Action.HomebrewAction' plus an @actions :: [Action]@ aggregate, which
a campaign's @Defs.hs@ folds into @Arkham.Homebrew.Defs.allActions@.
-}
declareHomebrewActions :: [String] -> Q [Dec]
declareHomebrewActions = declareOpenExtension ''Action 'HomebrewAction (Just "actions")

{- | @$(declareHomebrewConcealedCardKinds ["ThePhantom", ...])@ — pattern
synonyms over
'Arkham.Campaigns.TheScarletKeys.Concealed.Kind.HomebrewConcealedCardKind', each
tagged @":the-phantom-of-the-opera:the-phantom"@ — the box this module belongs
to, then the name, both kebab-cased. That keeps the tag unique across boxes and
is where the frontend reads the mini-card art from
(@frontend\/homebrew\/the-phantom-of-the-opera\/img\/mini-cards\/the-phantom.jpg@).
Pair each kind with the 'Arkham.Card.CardDef.CardDef' it conceals in
@Arkham.Homebrew.DefsBase.hdConcealedCardKinds@.
-}
declareHomebrewConcealedCardKinds :: [String] -> Q [Dec]
declareHomebrewConcealedCardKinds names = do
  box <- homebrewBoxId
  declareOpenExtensionWith
    (\n -> ":" <> box <> ":" <> kebabCase n)
    ''ConcealedCardKind
    'HomebrewConcealedCardKind
    Nothing
    names

{- | The splice site's box, kebab-cased: the id its encounter sets and card codes
are namespaced by, and the folder its art lives in. Taken from the module name
rather than asked for, so it cannot disagree with either.
-}
homebrewBoxId :: Q String
homebrewBoxId = do
  THS.Module _ (THS.ModName m) <- thisModule
  case List.break (== '.') <$> List.stripPrefix "Arkham.Homebrew." m of
    Just (box@(_ : _), '.' : _) -> pure (kebabCase box)
    _ -> fail $ "expected a module under Arkham.Homebrew.<Box>, got " <> m

kebabCase :: String -> String
kebabCase = map (\c -> if c == ' ' then '-' else Char.toLower c) . splitCamelCase

conNames :: Con -> [Name]
conNames = \case
  NormalC n _ -> [n]
  RecC n _ -> [n]
  InfixC _ n _ -> [n]
  ForallC _ _ c -> conNames c
  GadtC ns _ _ -> ns
  RecGadtC ns _ _ -> ns
