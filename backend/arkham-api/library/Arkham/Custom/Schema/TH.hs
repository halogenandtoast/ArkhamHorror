-- | Reifies the schema served by "Arkham.Custom.Schema".
module Arkham.Custom.Schema.TH (schemaFor, schemaForWith) where

import Arkham.Custom.Schema.Fields (conFieldNames)
import Arkham.Custom.Schema.Types
import Arkham.Custom.Schema.Windows (matcherWindows)
import Arkham.Prelude hiding (Type)
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax qualified as TH

{- | Types with a hand-written instance that serializes as a bare constructor
name, the way an all-nullary type would. The editor has to be told, or it sends
a tagged object that will not decode.
-}
stringEncodedTypes :: Set Text
stringEncodedTypes =
  Set.fromList
    [ "Action"
    , -- Nearly all nullary, but the open constructor for homebrew tokens carries
      -- a slug, so the all-nullary rule below cannot spot it. It still writes
      -- itself as a bare string.
      "ChaosTokenFace"
    ]

{- | Types the editor renders itself. Expanding them would add nothing and, in
the case of the id newtypes, would only expose the UUID inside.
-}
leafTypes :: Set Text
leafTypes =
  Set.fromList
    [ "Text"
    , "String"
    , "Int"
    , "Integer"
    , "Double"
    , "Bool"
    , "Char"
    , "Value"
    , "UUID"
    , "Message"
    , -- Newtypes over a code or a uuid. Expanding them would only expose the
      -- wrapper, where what the editor wants is a plain text field.
      "CardCode"
    , "InvestigatorId"
    , "EnemyId"
    , "LocationId"
    , "AssetId"
    , "TreacheryId"
    , "EventId"
    , "SkillId"
    , "StoryId"
    , "ActId"
    , "AgendaId"
    , "EffectId"
    , "ScenarioId"
    , "CampaignId"
    , "PlayerId"
    , "CardId"
    , "AbilityRef"
    , "BatchId"
    , -- Serializes as a bare string, and any unknown text is a homebrew trait,
      -- so a free text field beats a picker here.
      "Trait"
    ]

-- | Only Arkham's own types are worth expanding; anything else is a leaf.
expandable :: Name -> Bool
expandable n = case nameModule n of
  Just m -> "Arkham." `T.isPrefixOf` T.pack m && not (T.pack (nameBase n) `Set.member` leafTypes)
  Nothing -> False

renderType :: Type -> Text
renderType = \case
  ConT n -> T.pack (nameBase n)
  VarT n -> T.pack (nameBase n)
  ListT -> "[]"
  AppT ListT t -> "[" <> renderType t <> "]"
  AppT a b -> renderType a <> " " <> renderType b
  SigT t _ -> renderType t
  ParensT t -> renderType t
  TupleT n -> "(" <> T.replicate (max 0 (n - 1)) "," <> ")"
  t -> T.pack (show t)

referenced :: Type -> [Name]
referenced = \case
  ConT n -> [n]
  AppT a b -> referenced a <> referenced b
  SigT t _ -> referenced t
  ParensT t -> referenced t
  _ -> []

{- | Types encoded with @aesonOptions (Just <type>)@, which drops the lowercased
type name from the front of every field.

There is no way to read a type's aeson options back out in Template Haskell, and
the two conventions live side by side -- 'Modifier' strips, 'ChaosToken' and
'CardDraw' do not -- so which is which has to be written down. Reporting the
wrong one has the editor write @modifierType@ where the parser wants @type@, a
field that silently fails to decode. Add a type here only after checking its
'deriveJSON' call.
-}
prefixStrippedTypes :: Set Text
prefixStrippedTypes = Set.fromList ["Modifier", "Name", "Ability", "EffectBuilder", "CardOption"]

-- | A record field's JSON key.
jsonFieldName :: Name -> Name -> Text
jsonFieldName tyName fieldName
  | not (T.pack (nameBase tyName) `Set.member` prefixStrippedTypes) = field
  | otherwise = case T.stripPrefix prefix field of
      Just rest | not (T.null rest) -> uncapitalize rest
      _ -> field
 where
  field = T.pack (nameBase fieldName)
  prefix = uncapitalize (T.pack (nameBase tyName))
  uncapitalize t = case T.uncons t of
    Just (c, rest) -> T.toLower (T.singleton c) <> rest
    Nothing -> t

{- | The name written down for a positional field, if there is one.

See "Arkham.Custom.Schema.Fields". An empty name there means "leave it to the
type", which is the same as having no entry at all.
-}
positionalName :: Name -> Name -> Int -> Maybe Text
positionalName tyName conName' position = do
  names <- lookup (T.pack (nameBase tyName), T.pack (nameBase conName')) conFieldNames
  name <- names !!? position
  guard (not (T.null name))
  pure name

{- | The windows a matcher fires on, for 'WindowMatcher' and nothing else.

Carried on the constructor so the editor can name an ability's @$wN@ without
having to ask which window its matcher means -- the two types' names agree only
two thirds of the time. See "Arkham.Custom.Schema.Windows".
-}
windowsOf :: Name -> Name -> [Text]
windowsOf tyName conName'
  | nameBase tyName /= "WindowMatcher" = []
  | otherwise = fromMaybe [] (lookup (T.pack (nameBase conName')) matcherWindows)

conSchemas :: Name -> Con -> [(ConSchema, [Name])]
conSchemas tyName = \case
  NormalC n bts ->
    [
      ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema (positionalName tyName n i) (renderType t) | (i, (_, t)) <- zip [0 ..] bts]
          (windowsOf tyName n)
      , concatMap (referenced . snd) bts
      )
    ]
  RecC n vbts ->
    [
      ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema (Just (jsonFieldName tyName f)) (renderType t) | (f, _, t) <- vbts]
          (windowsOf tyName n)
      , concatMap (\(_, _, t) -> referenced t) vbts
      )
    ]
  InfixC a n b ->
    [
      ( ConSchema
          (T.pack (nameBase n))
          [ FieldSchema (positionalName tyName n 0) (renderType (snd a))
          , FieldSchema (positionalName tyName n 1) (renderType (snd b))
          ]
          (windowsOf tyName n)
      , referenced (snd a) <> referenced (snd b)
      )
    ]
  ForallC _ _ c -> conSchemas tyName c
  GadtC ns bts _ ->
    [ ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema (positionalName tyName n i) (renderType t) | (i, (_, t)) <- zip [0 ..] bts]
          (windowsOf tyName n)
      , concatMap (referenced . snd) bts
      )
    | n <- ns
    ]
  RecGadtC ns vbts _ ->
    [ ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema (Just (jsonFieldName tyName f)) (renderType t) | (f, _, t) <- vbts]
          (windowsOf tyName n)
      , concatMap (\(_, _, t) -> referenced t) vbts
      )
    | n <- ns
    ]

isRecordCon :: Con -> Bool
isRecordCon = \case
  RecC {} -> True
  RecGadtC {} -> True
  ForallC _ _ c -> isRecordCon c
  _ -> False

{- | Breadth-first over the reachable Arkham types, each visited once.

A shallow type has its constructors listed but does not enqueue the types they
mention. That is how 'Message' is included: the editor gets a picker over every
message, without dragging most of the codebase into the schema behind it.
-}
closure :: Set Text -> [Name] -> Set Text -> Q [TypeSchema]
closure _ [] _ = pure []
closure shallow (n : queue) seen
  | T.pack (nameBase n) `Set.member` seen = closure shallow queue seen
  | otherwise = do
      info <- reify n
      let seen' = Set.insert (T.pack (nameBase n)) seen
      case info of
        TyConI dec -> case dec of
          DataD _ _ _ _ constructors _ -> emit seen' constructors
          NewtypeD _ _ _ _ constructor _ -> emit seen' [constructor]
          TySynD _ _ rhs -> do
            let schema =
                  TypeSchema
                    { typeName = T.pack (nameBase n)
                    , typeConstructors = []
                    , typeIsRecord = False
                    , typeIsEnum = False
                    , typeAlias = Just (renderType rhs)
                    }
            rest <- closure shallow (queue <> filter expandable (referenced rhs)) seen'
            pure (schema : rest)
          _ -> closure shallow queue seen'
        _ -> closure shallow queue seen'
 where
  emit seen' constructors = do
    let entries = concatMap (conSchemas n) constructors
        schema =
          TypeSchema
            { typeName = T.pack (nameBase n)
            , typeConstructors =
                if T.pack (nameBase n) `Set.member` stringEncodedTypes
                  then filter (null . conFields) (map fst entries)
                  else map fst entries
            , typeIsRecord = any isRecordCon constructors
            , typeIsEnum =
                T.pack (nameBase n)
                  `Set.member` stringEncodedTypes
                  || all (null . conFields) (map fst entries)
            , typeAlias = Nothing
            }
        next
          | T.pack (nameBase n) `Set.member` shallow = []
          | otherwise = concatMap (filter expandable . snd) entries
    rest <- closure shallow (queue <> next) seen'
    pure (schema : rest)

-- | Splice the schema for these root types and everything they reach.
schemaFor :: [Name] -> Q Exp
schemaFor roots = schemaForWith roots []

-- | As 'schemaFor', with extra roots that are listed but not expanded through.
schemaForWith :: [Name] -> [Name] -> Q Exp
schemaForWith roots shallowRoots =
  closure (Set.fromList (map (T.pack . nameBase) shallowRoots)) (roots <> shallowRoots) mempty
    >>= TH.lift
