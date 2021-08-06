{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Asset where

import Arkham.Prelude

import Arkham.Types.Asset.Attrs hiding (assetEnemy, assetLocation)
import qualified Arkham.Types.Asset.Attrs as Attrs
import Arkham.Types.Asset.Cards
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Slot
import Arkham.Types.Trait (Trait)

$(buildEntity "Asset")

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a)

instance
  ( ActionRunner env
  , HasSkillTest env
  , CanCheckPlayable env
  )
  => HasActions env Asset where
  getActions iid window x = do
    inPlay <- member (toId x) <$> select AnyAsset
    modifiers' <- if inPlay
      then getModifiers (toSource x) (toTarget x)
      else pure []
    if Blank `elem` modifiers'
      then getActions iid window (toAttrs x)
      else defaultGetActions iid window x

instance
  ( HasId LocationId env InvestigatorId
  , HasId InvestigatorId env EventId
  , HasId CardCode env EnemyId
  , HasId (Maybe LocationId) env LocationMatcher
  , HasCount ResourceCount env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env EnemyId
  , HasCount ClueCount env InvestigatorId
  , HasCount AssetCount env (InvestigatorId, [Trait])
  , HasSet Trait env LocationId
  , HasSet CommittedCardId env InvestigatorId
  , HasSkillTest env
  )
  => HasModifiersFor env Asset where
  getModifiersFor = genericGetModifiersFor

instance
  ( HasList CommittedCard env InvestigatorId
  , HasId LeadInvestigatorId env ()
  , HasCount UsesCount env AssetId
  , CanCheckPlayable env
  , AssetRunner env
  )
  => RunMessage env Asset where
  runMessage msg x = do
    inPlay <- member (toId x) <$> select AnyAsset
    modifiers' <- if inPlay
      then getModifiers (toSource x) (toTarget x)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    genericRunMessage msg' x

instance HasCardCode Asset where
  toCardCode = toCardCode . toAttrs

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs

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

instance HasDamage Asset where
  getDamage = (assetHealthDamage &&& assetSanityDamage) . toAttrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . toAttrs

instance Discardable Asset where
  canBeDiscarded = and . sequence
    [assetCanLeavePlayByNormalMeans . toAttrs, not . cdPermanent . toCardDef]

instance HasId (Maybe OwnerId) env Asset where
  getId = pure . coerce . assetInvestigator . toAttrs

instance HasId (Maybe LocationId) env Asset where
  getId = pure . Attrs.assetLocation . toAttrs

instance HasCount DoomCount env Asset where
  getCount = pure . DoomCount . assetDoom . toAttrs

instance HasCount ClueCount env Asset where
  getCount = pure . ClueCount . assetClues . toAttrs

instance HasCount UsesCount env Asset where
  getCount x = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (toAttrs x)

instance HasCount StartingUsesCount env (Asset, UseType) where
  getCount (x, uType) = pure $ case uses' of
    Just (Uses uType' n) | uType == uType' -> StartingUsesCount n
    _ -> StartingUsesCount 0
    where uses' = assetStartingUses (toAttrs x)

instance HasCount UsesCount env (Asset, UseType) where
  getCount (x, uType) = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses uType' n | uType == uType' -> UsesCount n
    Uses _ _ -> UsesCount 0
    where uses' = assetUses (toAttrs x)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

allAssets :: Map CardCode (AssetId -> Asset)
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

isStory :: Asset -> Bool
isStory = assetIsStory . toAttrs

assetEnemy :: Asset -> Maybe EnemyId
assetEnemy = Attrs.assetEnemy . toAttrs

assetLocation :: Asset -> Maybe LocationId
assetLocation = Attrs.assetLocation . toAttrs
