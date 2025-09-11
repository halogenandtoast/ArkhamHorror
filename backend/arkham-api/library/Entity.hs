{-# LANGUAGE TemplateHaskellQuotes #-}

module Entity where

import Data.Char qualified as Char
import Data.Map.Lazy qualified as Map
import Data.Text (unpack)
import Database.Persist.Class.PersistEntity hiding (entityDef)
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi.Internal
import Database.Persist.TH
import Database.Persist.Types
import GHC.Records (HasField, getField)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude hiding (Type)

data HasFieldInstanceArgs = HasFieldInstanceArgs
  { modelName :: !Name
  , fieldName :: !Name
  , fieldType :: !Type
  , abbreviatedFieldName :: !String
  }
  deriving stock Show

data UnboundEntityDefWithTH = UnboundEntityDefWithTH
  { uedwTemplateHaskellName :: Name
  , uedwEntityDef :: UnboundEntityDef
  , uedwFieldDefs :: [UnboundFieldDefWithTH]
  }

data UnboundFieldDefWithTH = UnboundFieldDefWithTH
  { ufdwTemplateHaskellInfo :: Maybe (Name, Type)
  , ufdwFieldDef :: UnboundFieldDef
  }

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x : rest) = Char.toLower x : rest

upperFirst :: String -> String
upperFirst [] = []
upperFirst (c : cs) = Char.toUpper c : cs

mkEntity :: [EntityDef] -> [UnboundEntityDef] -> Q [Dec]
mkEntity preexistingEntities newEntities = do
  decs <- mkPersistWith sqlSettings preexistingEntities newEntities
  mconcat
    [ pure decs
    , generateVirtualFields newEntities decs
    ]

generateVirtualFields :: [UnboundEntityDef] -> [Dec] -> Q [Dec]
generateVirtualFields entityDefs decls = do
  defsWithTH <- entityDefsWithTH entityDefs decls
  mconcat
    [ foldMap fieldDefToHasFieldInstances (toHasFieldInstanceArgs defsWithTH)
    , foldMap decToIdHasFieldInstance decls
    ]

entityDefsWithTH :: [UnboundEntityDef] -> [Dec] -> Q [UnboundEntityDefWithTH]
entityDefsWithTH entityDefs decls = traverse entityDefWithTH entityDefs
 where
  entityDefWithTH :: UnboundEntityDef -> Q UnboundEntityDefWithTH
  entityDefWithTH entityDef = do
    let entityNameHS = entityHaskell $ unboundEntityDef entityDef
    let uedwTemplateHaskellName = mkName $ unpack $ unEntityNameHS entityNameHS
    fieldDefs <- traverse (fieldDefWithTH entityNameHS) (unboundEntityFields entityDef)
    pure
      UnboundEntityDefWithTH
        { uedwTemplateHaskellName
        , uedwEntityDef = entityDef
        , uedwFieldDefs = fieldDefs
        }
  fieldDefWithTH :: EntityNameHS -> UnboundFieldDef -> Q UnboundFieldDefWithTH
  fieldDefWithTH entityNameHS fieldDef = do
    ufdwTemplateHaskellInfo <-
      if FieldAttrMigrationOnly `elem` unboundFieldAttrs fieldDef
        then pure Nothing
        else do
          let entityName = unpack $ unEntityNameHS entityNameHS
          let fieldName = unpack $ unFieldNameHS $ unboundFieldNameHS fieldDef
          let fullFieldName = lowerFirst entityName <> upperFirst fieldName
          lookupWithError "Could not find record field declaration for field" fullFieldName thFieldMap
    pure UnboundFieldDefWithTH {ufdwTemplateHaskellInfo, ufdwFieldDef = fieldDef}
  lookupWithError :: String -> String -> Map String a -> Q (Maybe a)
  lookupWithError msg key map_ = do
    let result = Map.lookup key map_
    when (isNothing result) (reportError $ "entityDefsWithTH: " <> msg <> ": " <> key)
    pure result
  thFieldMap :: Map String (Name, Type)
  thFieldMap = mconcat do
    DataD _ _ _ _ [RecC _ varBangTypes] _ <- decls
    (fieldName, _strictness, fieldTYpe) <- varBangTypes
    pure $ Map.singleton (showName fieldName) (fieldName, fieldTYpe)

toHasFieldInstanceArgs :: [UnboundEntityDefWithTH] -> [HasFieldInstanceArgs]
toHasFieldInstanceArgs = concatMap go
 where
  go :: UnboundEntityDefWithTH -> [HasFieldInstanceArgs]
  go UnboundEntityDefWithTH {..} = flip mapMaybe uedwFieldDefs \UnboundFieldDefWithTH {..} -> do
    (fieldName, fieldType) <- ufdwTemplateHaskellInfo
    pure
      HasFieldInstanceArgs
        { modelName = uedwTemplateHaskellName
        , fieldName
        , fieldType
        , abbreviatedFieldName = unpack $ unFieldNameHS $ unboundFieldNameHS ufdwFieldDef
        }

fieldDefToHasFieldInstances :: HasFieldInstanceArgs -> Q [Dec]
fieldDefToHasFieldInstances HasFieldInstanceArgs {modelName, fieldName, fieldType, abbreviatedFieldName} = do
  let accessorFieldExpression :: Q Exp
      accessorFieldExpression = varE fieldName

      entityFieldConstr :: Q Exp
      entityFieldConstr = conE (mkName $ upperFirst $ showName fieldName)

      transformedAbbrFieldName :: String
      transformedAbbrFieldName = abbreviatedFieldName ++ ['_' | isReservedKeywordsDoesNotSupportDotField abbreviatedFieldName]

      accessorSymbol, entityType, resultType :: Q Type
      accessorSymbol = litT $ strTyLit transformedAbbrFieldName
      entityType = conT modelName
      resultType = pure fieldType

  symbolToFieldInstances <-
    if isReservedKeywordsDoesNotSupportDotField abbreviatedFieldName
      then
        [d|
          instance SymbolToField $accessorSymbol $entityType $resultType where
            symbolToField = $(entityFieldConstr)
          |]
      else pure []
  hasFieldInstances <-
    [d|
      instance HasField $accessorSymbol $entityType $resultType where
        getField record = $accessorFieldExpression record

      instance HasField $accessorSymbol (Entity $entityType) $resultType where
        getField (Entity _ record) = $accessorFieldExpression record
      |]
  pure (symbolToFieldInstances <> hasFieldInstances)

isReservedKeywordsDoesNotSupportDotField :: String -> Bool
isReservedKeywordsDoesNotSupportDotField str = str `elem` reservedKeywords

{- FOURMOLU_DISABLE -}
reservedKeywords :: [String]
reservedKeywords =
  [ "case" , "class" , "data" , "default" , "deriving" , "do" , "else" , "foreign" , "if"
  , "import" , "in" , "infix" , "infixl" , "infixr" , "instance" , "let" , "module" , "newtype"
  , "of" , "then" , "type" , "where"
  ]
{- FOURMOLU_ENABLE -}

decToIdHasFieldInstance :: Dec -> Q [Dec]
decToIdHasFieldInstance = \case
  DataD _ modelName _ _ _ _ -> do
    let entityType :: Q Type
        entityType = conT modelName
    [d|
      instance HasField "id" (Entity $entityType) (Key $entityType) where
        getField (Entity key _) = key
      |]
  _ -> pure []
