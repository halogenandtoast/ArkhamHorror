module Arkham.Asset.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasAbilities
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
import Arkham.Token (Token)
import Arkham.Trait (Trait)

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ AssetId, EntityAttrs a ~ AssetAttrs) => IsAsset a

type AssetCard a = CardBuilder (AssetId, Maybe InvestigatorId) a

newtype DiscardedAttrs a = DiscardedAttrs a

instance Entity a => Entity (DiscardedAttrs a) where
  type EntityId (DiscardedAttrs a) = EntityId a
  type EntityAttrs (DiscardedAttrs a) = EntityAttrs a
  toId (DiscardedAttrs a) = toId a
  toAttrs (DiscardedAttrs a) = toAttrs a
  overAttrs f (DiscardedAttrs a) = DiscardedAttrs $ overAttrs f a

data instance Field (DiscardedAttrs AssetAttrs) :: Type -> Type where
  DiscardedAssetTraits :: Field (DiscardedAttrs AssetAttrs) (HashSet Trait)

data instance Field AssetAttrs :: Type -> Type where
  AssetName :: Field AssetAttrs Name
  AssetCost :: Field AssetAttrs Int
  AssetClues :: Field AssetAttrs Int
  AssetHorror :: Field AssetAttrs Int
  AssetDamage :: Field AssetAttrs Int
  AssetRemainingHealth :: Field AssetAttrs (Maybe Int)
  AssetRemainingSanity :: Field AssetAttrs (Maybe Int)
  AssetDoom :: Field AssetAttrs Int
  AssetExhausted :: Field AssetAttrs Bool
  AssetUses :: Field AssetAttrs Uses
  AssetStartingUses :: Field AssetAttrs Uses
  AssetController :: Field AssetAttrs (Maybe InvestigatorId)
  AssetLocation :: Field AssetAttrs (Maybe LocationId)
  AssetCardCode :: Field AssetAttrs CardCode
  AssetSlots :: Field AssetAttrs [SlotType]
  AssetSealedTokens :: Field AssetAttrs [Token]
  AssetPlacement :: Field AssetAttrs Placement
  -- virtual
  AssetClasses :: Field AssetAttrs (HashSet ClassSymbol)
  AssetTraits :: Field AssetAttrs (HashSet Trait)
  AssetCardDef :: Field AssetAttrs CardDef
  AssetCard :: Field AssetAttrs Card
  AssetAbilities :: Field AssetAttrs [Ability]

data AssetAttrs = AssetAttrs
  { assetId :: AssetId
  , assetCardCode :: CardCode
  , assetOriginalCardCode :: CardCode
  , assetPlacement :: Placement
  , assetOwner :: Maybe InvestigatorId
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetUses :: Uses
  , assetExhausted :: Bool
  , assetDoom :: Int
  , assetClues :: Int
  , assetDamage :: Int
  , assetHorror :: Int
  , assetCanLeavePlayByNormalMeans :: Bool
  , assetDiscardWhenNoUses :: Bool
  , assetIsStory :: Bool
  , assetCardsUnderneath :: [Card]
  , assetSealedTokens :: [Token]
  }
  deriving stock (Show, Eq, Generic)

discardWhenNoUsesL :: Lens' AssetAttrs Bool
discardWhenNoUsesL =
  lens assetDiscardWhenNoUses $ \m x -> m {assetDiscardWhenNoUses = x}

canLeavePlayByNormalMeansL :: Lens' AssetAttrs Bool
canLeavePlayByNormalMeansL = lens assetCanLeavePlayByNormalMeans $
  \m x -> m {assetCanLeavePlayByNormalMeans = x}

sealedTokensL :: Lens' AssetAttrs [Token]
sealedTokensL = lens assetSealedTokens $ \m x -> m {assetSealedTokens = x}

horrorL :: Lens' AssetAttrs Int
horrorL = lens assetHorror $ \m x -> m {assetHorror = x}

isStoryL :: Lens' AssetAttrs Bool
isStoryL = lens assetIsStory $ \m x -> m {assetIsStory = x}

placementL :: Lens' AssetAttrs Placement
placementL = lens assetPlacement $ \m x -> m {assetPlacement = x}

cardsUnderneathL :: Lens' AssetAttrs [Card]
cardsUnderneathL = lens assetCardsUnderneath $ \m x -> m {assetCardsUnderneath = x}

healthL :: Lens' AssetAttrs (Maybe Int)
healthL = lens assetHealth $ \m x -> m {assetHealth = x}

sanityL :: Lens' AssetAttrs (Maybe Int)
sanityL = lens assetSanity $ \m x -> m {assetSanity = x}

slotsL :: Lens' AssetAttrs [SlotType]
slotsL = lens assetSlots $ \m x -> m {assetSlots = x}

doomL :: Lens' AssetAttrs Int
doomL = lens assetDoom $ \m x -> m {assetDoom = x}

cluesL :: Lens' AssetAttrs Int
cluesL = lens assetClues $ \m x -> m {assetClues = x}

usesL :: Lens' AssetAttrs Uses
usesL = lens assetUses $ \m x -> m {assetUses = x}

damageL :: Lens' AssetAttrs Int
damageL = lens assetDamage $ \m x -> m {assetDamage = x}

ownerL :: Lens' AssetAttrs (Maybe InvestigatorId)
ownerL = lens assetOwner $ \m x -> m {assetOwner = x}

exhaustedL :: Lens' AssetAttrs Bool
exhaustedL = lens assetExhausted $ \m x -> m {assetExhausted = x}

originalCardCodeL :: Lens' AssetAttrs CardCode
originalCardCodeL =
  lens assetOriginalCardCode $ \m x -> m {assetOriginalCardCode = x}

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
  toCard a = lookupCard (assetOriginalCardCode a) (toCardId a)
  toCardOwner = assetOwner

asset :: (AssetAttrs -> a) -> CardDef -> CardBuilder (AssetId, Maybe InvestigatorId) a
asset f cardDef = assetWith f cardDef id

ally :: (AssetAttrs -> a) -> CardDef -> (Int, Int) -> CardBuilder (AssetId, Maybe InvestigatorId) a
ally f cardDef stats = allyWith f cardDef stats id

allyWith ::
  (AssetAttrs -> a) ->
  CardDef ->
  (Int, Int) ->
  (AssetAttrs -> AssetAttrs) ->
  CardBuilder (AssetId, Maybe InvestigatorId) a
allyWith f cardDef (health, sanity) g =
  assetWith
    f
    cardDef
    (g . setSanity . setHealth)
 where
  setHealth = healthL .~ (health <$ guard (health > 0))
  setSanity = sanityL .~ (sanity <$ guard (sanity > 0))

assetWith ::
  (AssetAttrs -> a) ->
  CardDef ->
  (AssetAttrs -> AssetAttrs) ->
  CardBuilder (AssetId, Maybe InvestigatorId) a
assetWith f cardDef g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(aid, mOwner) ->
        f . g $
          AssetAttrs
            { assetId = aid
            , assetCardCode = toCardCode cardDef
            , assetOriginalCardCode = toCardCode cardDef
            , assetOwner = mOwner
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
  isSource AssetAttrs {assetId} (AssetSource aid) = assetId == aid
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

whenControlledBy ::
  Applicative m => AssetAttrs -> InvestigatorId -> m [Ability] -> m [Ability]
whenControlledBy a iid f = if controlledBy a iid then f else pure []

getOwner :: HasCallStack => AssetAttrs -> InvestigatorId
getOwner = fromJustNote "asset must be owned" . view ownerL

getController :: HasCallStack => AssetAttrs -> InvestigatorId
getController attrs = case assetPlacement attrs of
  InPlayArea iid -> iid
  _ -> error "asset must be controlled"

assetController :: AssetAttrs -> Maybe InvestigatorId
assetController attrs = case assetPlacement attrs of
  InPlayArea iid -> Just iid
  _ -> Nothing
