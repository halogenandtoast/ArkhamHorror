{-# LANGUAGE TemplateHaskell #-}
module Arkham.Asset where

import Arkham.Prelude

import Arkham.Asset.Assets
import Arkham.Asset.Attrs qualified as Attrs
import Arkham.Asset.Runner hiding (assetEnemy, assetLocation)
import Arkham.Card
import Arkham.Card.Id
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Name
import Arkham.Query
import Arkham.SkillTest
import Arkham.Slot
import Arkham.Trait (Trait)

$(buildEntity "Asset")

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a)

instance HasAbilities Asset where
  getAbilities = $(entityF "Asset" "getAbilities")

instance
  ( HasId LocationId env InvestigatorId
  , HasId InvestigatorId env EventId
  , HasId CardCode env EnemyId
  , HasCount ResourceCount env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env EnemyId
  , HasCount ClueCount env InvestigatorId
  , HasSet Trait env LocationId
  , HasSet CommittedCardId env InvestigatorId
  , HasCount RemainingSanity env InvestigatorId
  , HasId LocationId env AssetId
  , Query LocationMatcher env
  , Query AssetMatcher env
  , HasSkillTest env
  )
  => HasModifiersFor env Asset where
  getModifiersFor = $(entityF2 "Asset" "getModifiersFor")

instance AssetRunner env => RunMessage env Asset where
  runMessage msg x = do
    inPlay <- member (toId x) <$> select AnyAsset
    modifiers' <- if inPlay
      then getModifiers (toSource x) (toTarget x)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    $(entityRunMessage "Asset") msg' x

instance HasCardCode Asset where
  toCardCode = toCardCode . toAttrs

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Asset" "toAttrs")

instance Named Asset where
  toName = toName . toAttrs

instance HasName env Asset where
  getName = pure . toName

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardDef Asset where
  toCardDef = toCardDef . toAttrs

instance IsCard Asset where
  toCardId = toCardId . toAttrs
  toCard = toCard . toAttrs
  toCardOwner = toCardOwner . toAttrs

instance HasDamage Asset where
  getDamage = (assetHealthDamage &&& assetSanityDamage) . toAttrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . toAttrs

instance Discardable Asset where
  canBeDiscarded = and . sequence
    [assetCanLeavePlayByNormalMeans . toAttrs, not . cdPermanent . toCardDef]

instance HasId (Maybe LocationId) env Asset where
  getId = pure . Attrs.assetLocation . toAttrs

instance HasModifiersFor env () => HasCount DoomCount env Asset where
  getCount a = do
    modifiers <- getModifiers (toSource a) (toTarget a)
    let f = if DoomSubtracts `elem` modifiers then negate else id
    pure . DoomCount . f . assetDoom $ toAttrs a

instance HasCount HorrorCount env Asset where
  getCount = pure . HorrorCount . fromMaybe 0 . assetHorror . toAttrs

instance HasCount ClueCount env Asset where
  getCount = pure . ClueCount . assetClues . toAttrs

instance HasCount UsesCount env Asset where
  getCount x = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (toAttrs x)

instance HasCount StartingUsesCount env (Asset, UseType) where
  getCount (x, uType) = pure $ case uses' of
    Uses uType' n | uType == uType' -> StartingUsesCount n
    _ -> StartingUsesCount 0
    where uses' = cdUses $ toCardDef x

instance HasCount UsesCount env (Asset, UseType) where
  getCount (x, uType) = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses uType' n | uType == uType' -> UsesCount n
    Uses _ _ -> UsesCount 0
    where uses' = assetUses (toAttrs x)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  $(buildEntityLookupList "Asset")

slotsOf :: Asset -> [SlotType]
slotsOf = assetSlots . toAttrs

useTypeOf :: Asset -> Maybe UseType
useTypeOf = useType . assetUses . toAttrs

isHealthDamageable :: Asset -> Bool
isHealthDamageable a = case assetHealth (toAttrs a) of
  Nothing -> False
  Just n -> n > assetHealthDamage (toAttrs a)

isSanityDamageable :: Asset -> Bool
isSanityDamageable a = case assetSanity (toAttrs a) of
  Nothing -> False
  Just n -> n > assetSanityDamage (toAttrs a)

getRemainingAssetSanity :: Asset -> Int
getRemainingAssetSanity a = case assetSanity attrs of
  Nothing -> 0
  Just n -> max 0 $ n - assetSanityDamage attrs
  where attrs = toAttrs a

getRemainingAssetHealth :: Asset -> Int
getRemainingAssetHealth a = case assetHealth attrs of
  Nothing -> 0
  Just n -> max 0 $ n - assetHealthDamage attrs
  where attrs = toAttrs a

isStory :: Asset -> Bool
isStory = assetIsStory . toAttrs

assetEnemy :: Asset -> Maybe EnemyId
assetEnemy = Attrs.assetEnemy . toAttrs

assetLocation :: Asset -> Maybe LocationId
assetLocation = Attrs.assetLocation . toAttrs

assetOwner :: Asset -> Maybe InvestigatorId
assetOwner = Attrs.assetOwner . toAttrs

assetController :: Asset -> Maybe InvestigatorId
assetController = Attrs.assetController . toAttrs
