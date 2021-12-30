module Arkham.Asset.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Slot
import Arkham.Source
import Arkham.Target

class IsAsset a

type AssetCard a = CardBuilder AssetId a

data AssetAttrs = AssetAttrs
  { assetId :: AssetId
  , assetCardCode :: CardCode
  , assetOriginalCardCode :: CardCode
  , assetInvestigator :: Maybe InvestigatorId
  , assetLocation :: Maybe LocationId
  , assetEnemy :: Maybe EnemyId
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetHealthDamage :: Int
  , assetSanityDamage :: Int
  , assetUses :: Uses
  , assetExhausted :: Bool
  , assetDoom :: Int
  , assetClues :: Int
  , assetHorror :: Maybe Int
  , assetCanLeavePlayByNormalMeans :: Bool
  , assetDiscardWhenNoUses :: Bool
  , assetIsStory :: Bool
  }
  deriving stock (Show, Eq, Generic)

discardWhenNoUsesL :: Lens' AssetAttrs Bool
discardWhenNoUsesL =
  lens assetDiscardWhenNoUses $ \m x -> m {assetDiscardWhenNoUses = x}

canLeavePlayByNormalMeansL :: Lens' AssetAttrs Bool
canLeavePlayByNormalMeansL = lens assetCanLeavePlayByNormalMeans $
  \m x -> m {assetCanLeavePlayByNormalMeans = x}

horrorL :: Lens' AssetAttrs (Maybe Int)
horrorL = lens assetHorror $ \m x -> m {assetHorror = x}

isStoryL :: Lens' AssetAttrs Bool
isStoryL = lens assetIsStory $ \m x -> m {assetIsStory = x}

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

healthDamageL :: Lens' AssetAttrs Int
healthDamageL = lens assetHealthDamage $ \m x -> m {assetHealthDamage = x}

sanityDamageL :: Lens' AssetAttrs Int
sanityDamageL = lens assetSanityDamage $ \m x -> m {assetSanityDamage = x}

usesL :: Lens' AssetAttrs Uses
usesL = lens assetUses $ \m x -> m {assetUses = x}

locationL :: Lens' AssetAttrs (Maybe LocationId)
locationL = lens assetLocation $ \m x -> m {assetLocation = x}

enemyL :: Lens' AssetAttrs (Maybe EnemyId)
enemyL = lens assetEnemy $ \m x -> m {assetEnemy = x}

investigatorL :: Lens' AssetAttrs (Maybe InvestigatorId)
investigatorL = lens assetInvestigator $ \m x -> m {assetInvestigator = x}

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
  toEncoding = genericToEncoding $ aesonOptions $ Just "asset"

instance FromJSON AssetAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "asset"

instance IsCard AssetAttrs where
  toCardId = unAssetId . assetId
  toCard a = lookupCard (assetOriginalCardCode a) (toCardId a)

asset :: (AssetAttrs -> a) -> CardDef -> CardBuilder AssetId a
asset f cardDef = assetWith f cardDef id

ally :: (AssetAttrs -> a) -> CardDef -> (Int, Int) -> CardBuilder AssetId a
ally f cardDef stats = allyWith f cardDef stats id

allyWith ::
  (AssetAttrs -> a) ->
  CardDef ->
  (Int, Int) ->
  (AssetAttrs -> AssetAttrs) ->
  CardBuilder AssetId a
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
  CardBuilder AssetId a
assetWith f cardDef g =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \aid ->
        f . g $
          AssetAttrs
            { assetId = aid
            , assetCardCode = toCardCode cardDef
            , assetOriginalCardCode = toCardCode cardDef
            , assetInvestigator = Nothing
            , assetLocation = Nothing
            , assetEnemy = Nothing
            , assetSlots = cdSlots cardDef
            , assetHealth = Nothing
            , assetSanity = Nothing
            , assetHealthDamage = 0
            , assetSanityDamage = 0
            , assetUses = NoUses
            , assetExhausted = False
            , assetDoom = 0
            , assetClues = 0
            , assetHorror = Nothing
            , assetCanLeavePlayByNormalMeans = True
            , assetDiscardWhenNoUses = False
            , assetIsStory = False
            }
    }

instance Entity AssetAttrs where
  type EntityId AssetAttrs = AssetId
  type EntityAttrs AssetAttrs = AssetAttrs
  toId = assetId
  toAttrs = id

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

ownedBy :: AssetAttrs -> InvestigatorId -> Bool
ownedBy AssetAttrs {..} = (== assetInvestigator) . Just

whenOwnedBy ::
  Applicative m => AssetAttrs -> InvestigatorId -> m [Ability] -> m [Ability]
whenOwnedBy a iid f = if ownedBy a iid then f else pure []

getInvestigator :: HasCallStack => AssetAttrs -> InvestigatorId
getInvestigator = fromJustNote "asset must be owned" . view investigatorL

defeated :: AssetAttrs -> Bool
defeated AssetAttrs {..} =
  maybe False (assetHealthDamage >=) assetHealth
    || maybe False (assetSanityDamage >=) assetSanity
