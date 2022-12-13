module Arkham.Asset.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.ClassSymbol
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Token ( Token )
import Arkham.Trait ( Trait )
import Data.Constraint
import Data.Typeable

data Asset = forall a . IsAsset a => Asset a

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
  getModifiersFor target (Asset a) = getModifiersFor target a

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs
  toId = toId . toAttrs
  toAttrs (Asset a) = toAttrs a
  overAttrs f (Asset a) = Asset $ overAttrs f a

data SomeAssetCard = forall a . IsAsset a => SomeAssetCard (AssetCard a)

liftAssetCard :: (forall a . AssetCard a -> b) -> SomeAssetCard -> b
liftAssetCard f (SomeAssetCard a) = f a

someAssetCardCode :: SomeAssetCard -> CardCode
someAssetCardCode = liftAssetCard cbCardCode

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs


class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ AssetId, EntityAttrs a ~ AssetAttrs) => IsAsset a

type AssetCard a = CardBuilder (AssetId, Maybe InvestigatorId) a

data instance Field (DiscardedEntity Asset) :: Type -> Type where
  DiscardedAssetTraits :: Field (DiscardedEntity Asset) (HashSet Trait)

data instance Field Asset :: Type -> Type where
  AssetName :: Field Asset Name
  AssetCost :: Field Asset Int
  AssetClues :: Field Asset Int
  AssetHorror :: Field Asset Int
  AssetDamage :: Field Asset Int
  AssetRemainingHealth :: Field Asset (Maybe Int)
  AssetRemainingSanity :: Field Asset (Maybe Int)
  AssetDoom :: Field Asset Int
  AssetExhausted :: Field Asset Bool
  AssetUses :: Field Asset Uses
  AssetStartingUses :: Field Asset Uses
  AssetController :: Field Asset (Maybe InvestigatorId)
  AssetOwner :: Field Asset (Maybe InvestigatorId)
  AssetLocation :: Field Asset (Maybe LocationId)
  AssetCardCode :: Field Asset CardCode
  AssetSlots :: Field Asset [SlotType]
  AssetSealedTokens :: Field Asset [Token]
  AssetPlacement :: Field Asset Placement
  AssetCardsUnderneath :: Field Asset [Card]
  -- virtual
  AssetClasses :: Field Asset (HashSet ClassSymbol)
  AssetTraits :: Field Asset (HashSet Trait)
  AssetCardDef :: Field Asset CardDef
  AssetCard :: Field Asset Card
  AssetAbilities :: Field Asset [Ability]

deriving stock instance Show (Field Asset typ)

instance ToJSON (Field Asset typ) where
  toJSON = toJSON . show

instance (c Name, c Int, c (Maybe Int), c Bool, c Uses, c (Maybe InvestigatorId), c (Maybe LocationId), c CardCode, c [SlotType], c [Token], c Placement, c (HashSet ClassSymbol), c (HashSet Trait), c CardDef, c Card, c [Ability], c [Card]) => FieldDict c Asset where
  getDict = \case
    AssetName -> Dict
    AssetCost -> Dict
    AssetClues -> Dict
    AssetHorror -> Dict
    AssetDamage -> Dict
    AssetRemainingHealth -> Dict
    AssetRemainingSanity -> Dict
    AssetDoom -> Dict
    AssetExhausted -> Dict
    AssetUses -> Dict
    AssetStartingUses -> Dict
    AssetController -> Dict
    AssetOwner -> Dict
    AssetLocation -> Dict
    AssetCardCode -> Dict
    AssetSlots -> Dict
    AssetSealedTokens -> Dict
    AssetPlacement -> Dict
    AssetClasses -> Dict
    AssetTraits -> Dict
    AssetCardDef -> Dict
    AssetCard -> Dict
    AssetAbilities -> Dict
    AssetCardsUnderneath -> Dict

instance FromJSON (SomeField Asset) where
  parseJSON = withText "Field Asset" $ \case
    "AssetName" -> pure $ SomeField AssetName
    "AssetCost" -> pure $ SomeField AssetCost
    "AssetClues" -> pure $ SomeField AssetClues
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
    "AssetSlots" -> pure $ SomeField AssetSlots
    "AssetSealedTokens" -> pure $ SomeField AssetSealedTokens
    "AssetPlacement" -> pure $ SomeField AssetPlacement
    "AssetClasses" -> pure $ SomeField AssetClasses
    "AssetTraits" -> pure $ SomeField AssetTraits
    "AssetCardDef" -> pure $ SomeField AssetCardDef
    "AssetCard" -> pure $ SomeField AssetCard
    "AssetAbilities" -> pure $ SomeField AssetAbilities
    "AssetCardsUnderneath" -> pure $ SomeField AssetCardsUnderneath
    _ -> error "no such field"

data AssetAttrs = AssetAttrs
  { assetId :: AssetId
  , assetCardCode :: CardCode
  , assetOriginalCardCode :: CardCode
  , assetPlacement :: Placement
  , assetOwner :: Maybe InvestigatorId
  , assetController :: Maybe InvestigatorId
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetUses :: Uses
  , assetExhausted :: Bool
  , assetDoom :: Int
  , assetClues :: Int
  , assetDamage :: Int
  , assetHorror :: Int
  , assetResources :: Int
  , assetCanLeavePlayByNormalMeans :: Bool
  , assetDiscardWhenNoUses :: Bool
  , assetIsStory :: Bool
  , assetCardsUnderneath :: [Card]
  , assetSealedTokens :: [Token]
  }
  deriving stock (Show, Eq, Generic)

discardWhenNoUsesL :: Lens' AssetAttrs Bool
discardWhenNoUsesL =
  lens assetDiscardWhenNoUses $ \m x -> m { assetDiscardWhenNoUses = x }

canLeavePlayByNormalMeansL :: Lens' AssetAttrs Bool
canLeavePlayByNormalMeansL = lens assetCanLeavePlayByNormalMeans
  $ \m x -> m { assetCanLeavePlayByNormalMeans = x }

sealedTokensL :: Lens' AssetAttrs [Token]
sealedTokensL = lens assetSealedTokens $ \m x -> m { assetSealedTokens = x }

resourcesL :: Lens' AssetAttrs Int
resourcesL = lens assetResources $ \m x -> m { assetResources = x }

horrorL :: Lens' AssetAttrs Int
horrorL = lens assetHorror $ \m x -> m { assetHorror = x }

isStoryL :: Lens' AssetAttrs Bool
isStoryL = lens assetIsStory $ \m x -> m { assetIsStory = x }

placementL :: Lens' AssetAttrs Placement
placementL = lens assetPlacement $ \m x -> m { assetPlacement = x }

cardsUnderneathL :: Lens' AssetAttrs [Card]
cardsUnderneathL =
  lens assetCardsUnderneath $ \m x -> m { assetCardsUnderneath = x }

healthL :: Lens' AssetAttrs (Maybe Int)
healthL = lens assetHealth $ \m x -> m { assetHealth = x }

sanityL :: Lens' AssetAttrs (Maybe Int)
sanityL = lens assetSanity $ \m x -> m { assetSanity = x }

slotsL :: Lens' AssetAttrs [SlotType]
slotsL = lens assetSlots $ \m x -> m { assetSlots = x }

doomL :: Lens' AssetAttrs Int
doomL = lens assetDoom $ \m x -> m { assetDoom = x }

cluesL :: Lens' AssetAttrs Int
cluesL = lens assetClues $ \m x -> m { assetClues = x }

usesL :: Lens' AssetAttrs Uses
usesL = lens assetUses $ \m x -> m { assetUses = x }

damageL :: Lens' AssetAttrs Int
damageL = lens assetDamage $ \m x -> m { assetDamage = x }

ownerL :: Lens' AssetAttrs (Maybe InvestigatorId)
ownerL = lens assetOwner $ \m x -> m { assetOwner = x }

controllerL :: Lens' AssetAttrs (Maybe InvestigatorId)
controllerL = lens assetController $ \m x -> m { assetController = x }

exhaustedL :: Lens' AssetAttrs Bool
exhaustedL = lens assetExhausted $ \m x -> m { assetExhausted = x }

originalCardCodeL :: Lens' AssetAttrs CardCode
originalCardCodeL =
  lens assetOriginalCardCode $ \m x -> m { assetOriginalCardCode = x }

allAssetCards :: HashMap CardCode CardDef
allAssetCards =
  allPlayerAssetCards <> allEncounterAssetCards <> allSpecialPlayerAssetCards

instance HasCardCode AssetAttrs where
  toCardCode = assetCardCode

instance HasCardDef AssetAttrs where
  toCardDef a = case lookup (assetCardCode a) allAssetCards of
    Just def -> def
    Nothing -> error $ "missing card def for asset " <> show (assetCardCode a)

instance ToJSON AssetAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "asset"

instance FromJSON AssetAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "asset"

instance IsCard AssetAttrs where
  toCardId = unAssetId . assetId
  toCard a = case lookupCard (assetOriginalCardCode a) (toCardId a) of
    PlayerCard pc -> PlayerCard $ pc { pcOwner = assetOwner a }
    ec -> ec
  toCardOwner = assetOwner

asset
  :: (AssetAttrs -> a)
  -> CardDef
  -> CardBuilder (AssetId, Maybe InvestigatorId) a
asset f cardDef = assetWith f cardDef id

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
allyWith f cardDef (health, sanity) g = assetWith
  f
  cardDef
  (g . setSanity . setHealth)
 where
  setHealth = healthL .~ (health <$ guard (health > 0))
  setSanity = sanityL .~ (sanity <$ guard (sanity > 0))

assetWith
  :: (AssetAttrs -> a)
  -> CardDef
  -> (AssetAttrs -> AssetAttrs)
  -> CardBuilder (AssetId, Maybe InvestigatorId) a
assetWith f cardDef g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(aid, mOwner) -> f . g $ AssetAttrs
    { assetId = aid
    , assetCardCode = toCardCode cardDef
    , assetOriginalCardCode = toCardCode cardDef
    , assetOwner = mOwner
    , assetController = mOwner
    , assetPlacement = Unplaced
    , assetSlots = cdSlots cardDef
    , assetHealth = Nothing
    , assetSanity = Nothing
    , assetUses = NoUses
    , assetExhausted = False
    , assetDoom = 0
    , assetClues = 0
    , assetDamage = 0
    , assetHorror = 0
    , assetResources = 0
    , assetCanLeavePlayByNormalMeans = True
    , assetDiscardWhenNoUses = False
    , assetIsStory = False
    , assetCardsUnderneath = []
    , assetSealedTokens = []
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

instance TargetEntity AssetAttrs where
  toTarget = AssetTarget . toId
  isTarget attrs@AssetAttrs {..} = \case
    AssetTarget aid -> aid == assetId
    CardCodeTarget cardCode -> cdCardCode (toCardDef attrs) == cardCode
    CardIdTarget cardId -> cardId == unAssetId assetId
    SkillTestInitiatorTarget target -> isTarget attrs target
    _ -> False

instance SourceEntity AssetAttrs where
  toSource = AssetSource . toId
  isSource AssetAttrs { assetId } (AssetSource aid) = assetId == aid
  isSource _ _ = False

controlledBy :: AssetAttrs -> InvestigatorId -> Bool
controlledBy AssetAttrs {..} iid = case assetPlacement of
  InPlayArea iid' -> iid == iid'
  AttachedToAsset _ (Just (InPlayArea iid')) -> iid == iid'
  _ -> False

attachedToEnemy :: AssetAttrs -> EnemyId -> Bool
attachedToEnemy AssetAttrs {..} eid = case assetPlacement of
  AttachedToEnemy eid' -> eid == eid'
  _ -> False

whenControlledBy
  :: Applicative m => AssetAttrs -> InvestigatorId -> m [Ability] -> m [Ability]
whenControlledBy a iid f = if controlledBy a iid then f else pure []

getOwner :: HasCallStack => AssetAttrs -> InvestigatorId
getOwner = fromJustNote "asset must be owned" . view ownerL

getController :: HasCallStack => AssetAttrs -> InvestigatorId
getController = fromJustNote "asset must be controlled" . view controllerL
