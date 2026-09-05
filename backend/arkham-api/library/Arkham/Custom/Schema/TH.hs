-- | Reifies the schema served by "Arkham.Custom.Schema".
module Arkham.Custom.Schema.TH (schemaFor, schemaForWith) where

import Arkham.Custom.Schema.Types
import Arkham.Prelude hiding (Type)
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax qualified as TH

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

conSchemas :: Con -> [(ConSchema, [Name])]
conSchemas = \case
  NormalC n bts ->
    [
      ( ConSchema (T.pack (nameBase n)) [FieldSchema Nothing (renderType t) | (_, t) <- bts]
      , concatMap (referenced . snd) bts
      )
    ]
  RecC n vbts ->
    [
      ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema (Just (T.pack (nameBase f))) (renderType t) | (f, _, t) <- vbts]
      , concatMap (\(_, _, t) -> referenced t) vbts
      )
    ]
  InfixC a n b ->
    [
      ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema Nothing (renderType (snd a)), FieldSchema Nothing (renderType (snd b))]
      , referenced (snd a) <> referenced (snd b)
      )
    ]
  ForallC _ _ c -> conSchemas c
  GadtC ns bts _ ->
    [ ( ConSchema (T.pack (nameBase n)) [FieldSchema Nothing (renderType t) | (_, t) <- bts]
      , concatMap (referenced . snd) bts
      )
    | n <- ns
    ]
  RecGadtC ns vbts _ ->
    [ ( ConSchema
          (T.pack (nameBase n))
          [FieldSchema (Just (T.pack (nameBase f))) (renderType t) | (f, _, t) <- vbts]
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
    let entries = concatMap conSchemas constructors
        schema =
          TypeSchema
            { typeName = T.pack (nameBase n)
            , typeConstructors = map fst entries
            , typeIsRecord = any isRecordCon constructors
            , typeIsEnum = all (null . conFields) (map fst entries)
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
