{-# LANGUAGE TemplateHaskell #-}

module Arkham.Asset.Types where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Asset.Cards
import Arkham.Asset.Uses
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosToken)
import Arkham.ClassSymbol
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Customization
import Arkham.Field
import Arkham.Id
import Arkham.Json
import Arkham.Key
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher (replaceThisCard)
import Arkham.Matcher.Asset (AssetMatcher (AssetWithId))
import Arkham.Matcher.Base (Be (..))
import Arkham.Message
import Arkham.Name
import Arkham.Placement
import Arkham.Slot
import Arkham.Source
import Arkham.Taboo.Types
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Trait (Trait)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Data
import Data.Map.Strict qualified as Map
import GHC.Records

data Asset = forall a. IsAsset a => Asset a

instance Data Asset where
  gunfold _ _ _ = error "gunfold(Asset)"
  toConstr _ = error "toConstr(Asset)"
  dataTypeOf _ = error "dataTypeOf(Asset)"

instance Data (SomeField Asset) where
  gunfold _ _ _ = error "gunfold(Asset)"
  toConstr _ = error "toConstr(Asset)"
  dataTypeOf _ = error "dataTypeOf(Asset)"

instance Typeable a => Data (Field Asset a) where
  gunfold _ _ _ = error "gunfold(Asset)"
  toConstr _ = error "toConstr(Asset)"
  dataTypeOf _ = error "dataTypeOf(Asset)"

instance AsId Asset where
  type IdOf Asset = AssetId
  asId = toId

instance AsId AssetAttrs where
  type IdOf AssetAttrs = AssetId
  asId = toId

instance Named Asset where
  toName (Asset a) = toName (toAttrs a)
  {-# INLINE toName #-}

instance HasCardDef Asset where
  toCardDef (Asset a) = toCardDef (toAttrs a)
  {-# INLINE toCardDef #-}

instance HasCardCode Asset where
  toCardCode (Asset a) = toCardCode (toAttrs a)
  {-# INLINE toCardCode #-}

instance IsCard Asset where
  toCard (Asset a) = toCard (toAttrs a)
  {-# INLINE toCard #-}
  toCardId (Asset a) = toCardId (toAttrs a)
  {-# INLINE toCardId #-}
  toCardOwner (Asset a) = toCardOwner (toAttrs a)
  {-# INLINE toCardOwner #-}
  toCustomizations (Asset a) = toCustomizations (toAttrs a)
  {-# INLINE toCustomizations #-}

instance Eq Asset where
  Asset (a :: a) == Asset (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Asset where
  show (Asset a) = show a

instance ToJSON Asset where
  toJSON (Asset a) = toJSON a

instance HasAbilities Asset where
  getAbilities (Asset a) = getAbilities a

instance HasModifiersFor Asset where
  getModifiersFor (Asset a) = getModifiersFor a

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs
  toId = toId . toAttrs
  toAttrs (Asset a) = toAttrs a
  overAttrs f (Asset a) = Asset $ overAttrs f a

data SomeAssetCard = forall a. IsAsset a => SomeAssetCard (AssetCard a)

liftAssetCard :: (forall a. AssetCard a -> b) -> SomeAssetCard -> b
liftAssetCard f (SomeAssetCard a) = f a

someAssetCardCode :: SomeAssetCard -> CardCode
someAssetCardCode = liftAssetCard cbCardCode

instance Targetable Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ AssetId
  , EntityAttrs a ~ AssetAttrs
  ) =>
  IsAsset a

type AssetCard a = CardBuilder (AssetId, Maybe InvestigatorId) a

data instance Field (DiscardedEntity Asset) :: Type -> Type where
  DiscardedAssetTraits :: Field (DiscardedEntity Asset) (Set Trait)
  DiscardedAssetController :: Field (DiscardedEntity Asset) (Maybe InvestigatorId)

data instance Field (InHandEntity Asset) :: Type -> Type where
  InHandAssetCardId :: Field (InHandEntity Asset) CardId

data instance Field (InDiscardEntity Asset) :: Type -> Type where
  InDiscardAssetCardId :: Field (InDiscardEntity Asset) CardId

data instance Field Asset :: Type -> Type where
  AssetTokens :: Field Asset Tokens
  AssetName :: Field Asset Name
  AssetCost :: Field Asset Int
  AssetClues :: Field Asset Int
  AssetResources :: Field Asset Int
  AssetHorror :: Field Asset Int
  AssetDamage :: Field Asset Int
  AssetRemainingHealth :: Field Asset (Maybe Int)
  AssetRemainingSanity :: Field Asset (Maybe Int)
  AssetDoom :: Field Asset Int
  AssetExhausted :: Field Asset Bool
  AssetUses :: Field Asset (Map UseType Int)
  AssetStartingUses :: Field Asset (Uses GameCalculation)
  AssetController :: Field Asset (Maybe InvestigatorId)
  AssetOwner :: Field Asset (Maybe InvestigatorId)
  AssetLocation :: Field Asset (Maybe LocationId)
  AssetCardCode :: Field Asset CardCode
  AssetCardId :: Field Asset CardId
  AssetSlots :: Field Asset [SlotType]
  AssetSealedChaosTokens :: Field Asset [ChaosToken]
  AssetPlacement :: Field Asset Placement
  AssetCardsUnderneath :: Field Asset [Card]
  AssetCustomizations :: Field Asset Customizations
  AssetAssignedHealthHeal :: Field Asset (Map Source Int)
  AssetAssignedSanityHeal :: Field Asset (Map Source Int)
  AssetAssignedHealthDamage :: Field Asset Int
  AssetAssignedSanityDamage :: Field Asset Int
  AssetDriver :: Field Asset (Maybe InvestigatorId)
  -- virtual
  AssetClasses :: Field Asset (Set ClassSymbol)
  AssetTraits :: Field Asset (Set Trait)
  AssetCardDef :: Field Asset CardDef
  AssetCard :: Field Asset Card
  AssetAbilities :: Field Asset [Ability]

deriving stock instance Show (Field Asset typ)
deriving stock instance Ord (Field Asset typ)

instance ToJSON (Field Asset typ) where
  toJSON = toJSON . show

instance Typeable typ => FromJSON (Field Asset typ) where
  parseJSON x = do
    z <- parseJSON @(SomeField Asset) x
    case z of
      SomeField (f :: Field Asset k) -> case eqT @typ @k of
        Just Refl -> pure f
        Nothing -> error "type mismatch"

instance FromJSON (SomeField Asset) where
  parseJSON = withText "Field Asset" $ \case
    "AssetName" -> pure $ SomeField AssetName
    "AssetCost" -> pure $ SomeField AssetCost
    "AssetClues" -> pure $ SomeField AssetClues
    "AssetResources" -> pure $ SomeField AssetResources
    "AssetHorror" -> pure $ SomeField AssetHorror
    "AssetDamage" -> pure $ SomeField AssetDamage
    "AssetRemainingHealth" -> pure $ SomeField AssetRemainingHealth
    "AssetRemainingSanity" -> pure $ SomeField AssetRemainingSanity
    "AssetDoom" -> pure $ SomeField AssetDoom
    "AssetExhausted" -> pure $ SomeField AssetExhausted
    "AssetUses" -> pure $ SomeField AssetUses
    "AssetStartingUses" -> pure $ SomeField AssetStartingUses
    "AssetController" -> pure $ SomeField AssetController
    "AssetOwner" -> pure $ SomeField AssetOwner
    "AssetLocation" -> pure $ SomeField AssetLocation
    "AssetCardCode" -> pure $ SomeField AssetCardCode
    "AssetCardId" -> pure $ SomeField AssetCardId
    "AssetSlots" -> pure $ SomeField AssetSlots
    "AssetSealedChaosTokens" -> pure $ SomeField AssetSealedChaosTokens
    "AssetPlacement" -> pure $ SomeField AssetPlacement
    "AssetClasses" -> pure $ SomeField AssetClasses
    "AssetTraits" -> pure $ SomeField AssetTraits
    "AssetCardDef" -> pure $ SomeField AssetCardDef
    "AssetCard" -> pure $ SomeField AssetCard
    "AssetAbilities" -> pure $ SomeField AssetAbilities
    "AssetCardsUnderneath" -> pure $ SomeField AssetCardsUnderneath
    "AssetCustomizations" -> pure $ SomeField AssetCustomizations
    "AssetAssignedHealthHeal" -> pure $ SomeField AssetAssignedHealthHeal
    "AssetAssignedSanityHeal" -> pure $ SomeField AssetAssignedSanityHeal
    "AssetAssignedHealthDamage" -> pure $ SomeField AssetAssignedHealthDamage
    "AssetAssignedSanityDamage" -> pure $ SomeField AssetAssignedSanityDamage
    "AssetTokens" -> pure $ SomeField AssetTokens
    "AssetDriver" -> pure $ SomeField AssetDriver
    _ -> error "no such field"

data WhenNoUses = DiscardWhenNoUses | ReturnToHandWhenNoUses | NotifySelfOfNoUses
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AssetAttrs = AssetAttrs
  { assetId :: AssetId
  , assetCardId :: CardId
  , assetCardCode :: CardCode
  , assetOriginalCardCode :: CardCode
  , assetPlacement :: Placement
  , assetOwner :: Maybe InvestigatorId
  , assetController :: Maybe InvestigatorId
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetPrintedUses :: Uses GameCalculation
  , assetExhausted :: Bool
  , assetExiled :: Bool
  , assetTokens :: Tokens
  , assetCanLeavePlayByNormalMeans :: Bool
  , assetWhenNoUses :: Maybe WhenNoUses
  , assetIsStory :: Bool
  , assetCardsUnderneath :: [Card]
  , assetSealedChaosTokens :: [ChaosToken]
  , assetKeys :: Set ArkhamKey
  , assetAssignedHealthDamage :: Int
  , assetAssignedSanityDamage :: Int
  , assetAssignedHealthHeal :: Map Source Int
  , assetAssignedSanityHeal :: Map Source Int
  , assetCustomizations :: Customizations
  , assetMeta :: Value
  , assetFlipped :: Bool
  , assetTaboo :: Maybe TabooList
  , assetMutated :: Maybe Text -- for art display
  , assetDriver :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq)

assetUses :: AssetAttrs -> Map UseType Int
assetUses = Map.filterWithKey (\k _ -> tokenIsUse k) . coerce . assetTokens

instance Is AssetAttrs AssetId where
  is = (==) . toId
  {-# INLINE is #-}

instance Be AssetAttrs AssetMatcher where
  be = AssetWithId . assetId

instance HasField "placement" Asset Placement where
  getField = (.placement) . toAttrs

instance HasField "id" Asset AssetId where
  getField = (.id) . toAttrs

instance HasField "owner" Asset (Maybe InvestigatorId) where
  getField = (.owner) . toAttrs

instance HasField "taboo" AssetAttrs (Maybe TabooList) where
  getField = assetTaboo

instance HasField "name" AssetAttrs Name where
  getField = toName

instance HasField "driver" AssetAttrs (Maybe InvestigatorId) where
  getField = assetDriver

instance HasField "customizations" AssetAttrs Customizations where
  getField = assetCustomizations

instance HasField "cardId" AssetAttrs CardId where
  getField = assetCardId

instance HasField "id" AssetAttrs AssetId where
  getField = assetId

instance HasField "meta" AssetAttrs Value where
  getField = assetMeta

instance HasField "flipped" AssetAttrs Bool where
  getField = assetFlipped

instance HasField "placement" AssetAttrs Placement where
  getField = assetPlacement

instance HasField "exiled" AssetAttrs Bool where
  getField = assetExiled

instance HasField "exhausted" AssetAttrs Bool where
  getField = assetExhausted

instance HasField "ready" AssetAttrs Bool where
  getField = not . assetExhausted

instance HasField "horror" AssetAttrs Int where
  getField = assetHorror

instance HasField "doom" AssetAttrs Int where
  getField = assetDoom

instance HasField "controller" AssetAttrs (Maybe InvestigatorId) where
  getField = assetController

instance HasField "owner" AssetAttrs (Maybe InvestigatorId) where
  getField = assetOwner

instance HasField "uses" AssetAttrs (Map UseType Int) where
  getField = Map.filterWithKey (\k _ -> tokenIsUse k) . coerce . assetTokens

instance HasField "tokens" AssetAttrs Tokens where
  getField = assetTokens

instance HasField "sealedChaosTokens" AssetAttrs [ChaosToken] where
  getField = assetSealedChaosTokens

instance HasField "cardsUnderneath" AssetAttrs [Card] where
  getField = assetCardsUnderneath

instance HasField "use" AssetAttrs (UseType -> Int) where
  getField a uType = findWithDefault 0 uType a.uses

instance HasField "token" AssetAttrs (Token -> Int) where
  getField a tType = countTokens tType a.tokens

instance HasField "ability" AssetAttrs (Int -> Source) where
  getField this = toAbilitySource this

instance HasField "damage" AssetAttrs Int where
  getField = assetDamage

instance HasField "inThreatAreaOf" AssetAttrs (Maybe InvestigatorId) where
  getField attrs = case attrs.placement of
    InThreatArea iid -> Just iid
    _ -> Nothing

assetDoom :: AssetAttrs -> Int
assetDoom = countTokens Doom . assetTokens

assetClues :: AssetAttrs -> Int
assetClues = countTokens Clue . assetTokens

assetDamage :: AssetAttrs -> Int
assetDamage = countTokens Damage . assetTokens

assetHorror :: AssetAttrs -> Int
assetHorror = countTokens Horror . assetTokens

assetResources :: AssetAttrs -> Int
assetResources = countTokens Token.Resource . assetTokens

allAssetCards :: Map CardCode CardDef
allAssetCards =
  allPlayerAssetCards <> allEncounterAssetCards <> allSpecialPlayerAssetCards

instance HasCardCode AssetAttrs where
  toCardCode = assetCardCode

instance HasCardCode (With AssetAttrs meta) where
  toCardCode (With x _) = assetCardCode x

instance HasCardDef AssetAttrs where
  toCardDef a = case lookup (assetCardCode a) allAssetCards of
    Just def -> def
    Nothing -> error $ "missing card def for asset " <> show (assetCardCode a)

instance IsCard AssetAttrs where
  toCardId = assetCardId
  toCard a = case lookupCard (assetOriginalCardCode a) (toCardId a) of
    PlayerCard pc ->
      PlayerCard
        $ pc
          { pcOwner = assetOwner a
          , pcCustomizations = toCustomizations a
          , pcTabooList = assetTaboo a
          , pcMutated = assetMutated a
          }
    ec -> ec
  toCardOwner = assetOwner
  toCustomizations = assetCustomizations

asset
  :: (AssetAttrs -> a)
  -> CardDef
  -> CardBuilder (AssetId, Maybe InvestigatorId) a
asset f cardDef = assetWith f cardDef id

assetWith
  :: (AssetAttrs -> a)
  -> CardDef
  -> (AssetAttrs -> AssetAttrs)
  -> CardBuilder (AssetId, Maybe InvestigatorId) a
assetWith f cardDef g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (aid, mOwner) ->
        f
          . g
          $ AssetAttrs
            { assetId = aid
            , assetCardId = cardId
            , assetCardCode = toCardCode cardDef
            , assetOriginalCardCode = toCardCode cardDef
            , assetOwner = mOwner
            , assetController = Nothing
            , assetPlacement = Unplaced
            , assetSlots = cdSlots cardDef
            , assetHealth = Nothing
            , assetSanity = Nothing
            , assetPrintedUses = replaceThisCard cardId (cdUses cardDef)
            , assetExhausted = False
            , assetExiled = False
            , assetTokens = mempty
            , assetCanLeavePlayByNormalMeans = Keyword.Partner `notElem` cdKeywords cardDef
            , assetWhenNoUses = Nothing
            , assetIsStory = False
            , assetCardsUnderneath = []
            , assetSealedChaosTokens = []
            , assetKeys = mempty
            , assetAssignedHealthDamage = 0
            , assetAssignedHealthHeal = mempty
            , assetAssignedSanityDamage = 0
            , assetAssignedSanityHeal = mempty
            , assetCustomizations = mempty
            , assetMeta = Null
            , assetFlipped = False
            , assetTaboo = Nothing
            , assetMutated = Nothing
            , assetDriver = Nothing
            }
    }

instance Entity AssetAttrs where
  type EntityId AssetAttrs = AssetId
  type EntityAttrs AssetAttrs = AssetAttrs
  toId = assetId
  toAttrs = id
  overAttrs f = f

instance Named AssetAttrs where
  toName = toName . toCardDef

instance Targetable AssetAttrs where
  toTarget = AssetTarget . toId
  isTarget attrs@AssetAttrs {..} = \case
    AssetTarget aid -> aid == assetId
    CardCodeTarget cardCode -> cdCardCode (toCardDef attrs) == cardCode
    CardIdTarget cardId -> cardId == assetCardId
    SkillTestInitiatorTarget target -> isTarget attrs target
    _ -> False

instance Sourceable AssetAttrs where
  toSource = AssetSource . toId
  isSource AssetAttrs {assetId} (AssetSource aid) = assetId == aid
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource _ _ = False

controls :: (Entity attrs, EntityAttrs attrs ~ AssetAttrs) => InvestigatorId -> attrs -> Bool
controls iid attrs = toAttrs attrs `controlledBy` iid

controlledBy :: AssetAttrs -> InvestigatorId -> Bool
controlledBy AssetAttrs {..} iid =
  if isInPlayPlacement assetPlacement
    then case assetController of
      Nothing -> False
      Just iid' -> iid == iid'
    else False

ownedBy :: AssetAttrs -> InvestigatorId -> Bool
ownedBy a iid = a.owner == Just iid

attachedToEnemy :: AssetAttrs -> EnemyId -> Bool
attachedToEnemy AssetAttrs {..} eid = case assetPlacement of
  AttachedToEnemy eid' -> eid == eid'
  _ -> False

whenControlledBy
  :: Applicative m => AssetAttrs -> InvestigatorId -> m [Ability] -> m [Ability]
whenControlledBy a iid f = if controlledBy a iid then f else pure []

makeLensesWith suffixedFields ''AssetAttrs

getOwner :: HasCallStack => AssetAttrs -> InvestigatorId
getOwner = fromJustNote "asset must be owned" . view ownerL

getController :: HasCallStack => AssetAttrs -> InvestigatorId
getController = fromJustNote "asset must be controlled" . view controllerL

noSlots :: AssetAttrs -> AssetAttrs
noSlots = slotsL .~ mempty

ally
  :: (AssetAttrs -> a)
  -> CardDef
  -> (Int, Int)
  -> CardBuilder (AssetId, Maybe InvestigatorId) a
ally f cardDef stats = allyWith f cardDef stats id

allyWith
  :: (AssetAttrs -> a)
  -> CardDef
  -> (Int, Int)
  -> (AssetAttrs -> AssetAttrs)
  -> CardBuilder (AssetId, Maybe InvestigatorId) a
allyWith f cardDef (health, sanity) g =
  assetWith
    f
    cardDef
    (g . setSanity . setHealth)
 where
  setHealth = healthL .~ (health <$ guard (health > 0))
  setSanity = sanityL .~ (sanity <$ guard (sanity > 0))

discardWhenNoUses :: AssetAttrs -> AssetAttrs
discardWhenNoUses = whenNoUsesL ?~ DiscardWhenNoUses

setMeta :: ToJSON a => a -> AssetAttrs -> AssetAttrs
setMeta a = metaL .~ toJSON a

getAssetMeta :: FromJSON a => AssetAttrs -> Maybe a
getAssetMeta attrs = case fromJSON attrs.meta of
  Error _ -> Nothing
  Success v' -> Just v'

unsetMeta :: AssetAttrs -> AssetAttrs
unsetMeta = metaL .~ Null

getAssetMetaDefault :: FromJSON a => a -> AssetAttrs -> a
getAssetMetaDefault def = fromMaybe def . getAssetMeta

overMeta :: (ToJSON a, FromJSON a) => (a -> a -> a) -> a -> AssetAttrs -> AssetAttrs
overMeta f a attrs = case fromJSON attrs.meta of
  Error _ -> attrs & metaL .~ toJSON a
  Success a' -> attrs & metaL .~ toJSON (f a' a)

setMetaKey :: (ToJSON a, HasCallStack) => Key -> a -> AssetAttrs -> AssetAttrs
setMetaKey k v attrs = case attrs.meta of
  Object o -> attrs {assetMeta = Object $ KeyMap.insert k (toJSON v) o}
  Null -> attrs {assetMeta = object [k .= v]}
  _ -> error $ "Could not insert meta key, meta is not Null or Object: " <> show attrs.meta

unsetMetaKey :: Key -> AssetAttrs -> AssetAttrs
unsetMetaKey k attrs = case attrs.meta of
  Object o -> attrs {assetMeta = Object $ KeyMap.delete k o}
  _ -> attrs

getMetaKey :: Key -> AssetAttrs -> Bool
getMetaKey k attrs = case attrs.meta of
  Object o -> case KeyMap.lookup k o of
    Nothing -> False
    Just v -> case fromJSON v of
      Error _ -> False
      Success v' -> v'
  _ -> False

getMetaKeyDefault :: FromJSON a => Key -> a -> AssetAttrs -> a
getMetaKeyDefault k def attrs = case attrs.meta of
  Object o -> case KeyMap.lookup k o of
    Nothing -> def
    Just v -> case fromJSON v of
      Error _ -> def
      Success v' -> v'
  _ -> def

$(deriveJSON (aesonOptions $ Just "asset") ''AssetAttrs)
