{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Asset.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Action
import Arkham.Types.Asset.Class
import Arkham.Types.Asset.Uses

type AssetCard a = (AssetId -> a)

data AssetAttrs = AssetAttrs
  { assetId :: AssetId
  , assetCardDef :: CardDef
  , assetInvestigator :: Maybe InvestigatorId
  , assetLocation :: Maybe LocationId
  , assetEnemy :: Maybe EnemyId
  , assetActions :: [Message]
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
  , assetIsStory :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''AssetAttrs

instance HasCardDef AssetAttrs where
  toCardDef = assetCardDef

instance ToJSON AssetAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "asset"
  toEncoding = genericToEncoding $ aesonOptions $ Just "asset"

instance FromJSON AssetAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "asset"

instance IsCard AssetAttrs where
  toCardId = unAssetId . assetId

asset :: (AssetAttrs -> a) -> CardDef -> AssetId -> a
asset f cardDef aid = assetWith f cardDef id aid

ally :: (AssetAttrs -> a) -> CardDef -> (Int, Int) -> AssetId -> a
ally f cardDef stats  = allyWith f cardDef stats id

allyWith :: (AssetAttrs -> a) -> CardDef -> (Int, Int) -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
allyWith f cardDef (health, sanity) g = slotWith AllySlot f cardDef (g . setSanity . setHealth)
 where
   setHealth = healthL .~ (health <$ guard (health > 0))
   setSanity = sanityL .~ (sanity <$ guard (sanity > 0))

arcane :: (AssetAttrs -> a) -> CardDef -> AssetId -> a
arcane f cardDef = arcaneWith f cardDef id

arcaneWith :: (AssetAttrs -> a) -> CardDef -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
arcaneWith = slotWith ArcaneSlot

body :: (AssetAttrs -> a) -> CardDef -> AssetId -> a
body f cardDef = bodyWith f cardDef id

bodyWith :: (AssetAttrs -> a) -> CardDef -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
bodyWith = slotWith BodySlot

accessory :: (AssetAttrs -> a) -> CardDef -> AssetId -> a
accessory f cardDef = accessoryWith f cardDef id

accessoryWith :: (AssetAttrs -> a) -> CardDef -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
accessoryWith = slotWith AccessorySlot

hand :: (AssetAttrs -> a) -> CardDef -> AssetId -> a
hand f cardDef = handWith f cardDef id

handWith :: (AssetAttrs -> a) -> CardDef -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
handWith = slotWith HandSlot

slotWith :: SlotType -> (AssetAttrs -> a) -> CardDef -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
slotWith slot f cardDef g aid = assetWith f cardDef (g . (slotsL .~ [slot])) aid

assetWith :: (AssetAttrs -> a) -> CardDef -> (AssetAttrs -> AssetAttrs) -> AssetId -> a
assetWith f cardDef g aid = f . g $ AssetAttrs
  { assetId = aid
  , assetCardDef = cardDef
  , assetInvestigator = Nothing
  , assetLocation = Nothing
  , assetEnemy = Nothing
  , assetActions = mempty
  , assetSlots = mempty
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
  , assetIsStory = False
  }

instance Entity AssetAttrs where
  type EntityId AssetAttrs = AssetId
  type EntityAttrs AssetAttrs = AssetAttrs
  toId = assetId
  toAttrs = id

instance NamedEntity AssetAttrs where
  toName = cdName . toCardDef

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
  isSource attrs (PlayerCardSource cid) = toCardId attrs == cid
  isSource _ _ = False

ownedBy :: AssetAttrs -> InvestigatorId -> Bool
ownedBy AssetAttrs {..} = (== assetInvestigator) . Just

whenOwnedBy
  :: Applicative m => AssetAttrs -> InvestigatorId -> m [Message] -> m [Message]
whenOwnedBy a iid f = if ownedBy a iid then f else pure []

assetAction
  :: InvestigatorId -> AssetAttrs -> Int -> Maybe Action -> Cost -> Message
assetAction iid attrs idx mAction cost =
  ActivateCardAbilityAction iid
    $ mkAbility (toSource attrs) idx (ActionAbility mAction cost)

getInvestigator :: HasCallStack => AssetAttrs -> InvestigatorId
getInvestigator = fromJustNote "asset must be owned" . view investigatorL

defeated :: AssetAttrs -> Bool
defeated AssetAttrs {..} =
  maybe False (assetHealthDamage >=) assetHealth
    || maybe False (assetSanityDamage >=) assetSanity

instance HasActions env AssetAttrs where
  getActions _ _ _ = pure []

instance IsAsset AssetAttrs where
  slotsOf = assetSlots
  useTypeOf = useType . assetUses
  isHealthDamageable a = case assetHealth a of
    Nothing -> False
    Just n -> n > assetHealthDamage a
  isSanityDamageable a = case assetSanity a of
    Nothing -> False
    Just n -> n > assetSanityDamage a
  isStory = assetIsStory

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env AssetAttrs where
  runMessage msg a@AssetAttrs {..} = case msg of
    ReadyExhausted -> case assetInvestigator of
      Just iid -> do
        modifiers <-
          map modifierType
            <$> getModifiersFor (toSource a) (InvestigatorTarget iid) ()
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhaustedL .~ False
      Nothing -> pure $ a & exhaustedL .~ False
    RemoveAllDoom -> pure $ a & doomL .~ 0
    PlaceClues target n | isTarget a target -> pure $ a & cluesL +~ n
    RemoveClues target n | isTarget a target ->
      pure $ a & cluesL %~ max 0 . subtract n
    CheckDefeated _ ->
      a <$ when (defeated a) (unshiftMessages $ resolve $ AssetDefeated assetId)
    AssetDefeated aid | aid == assetId ->
      a <$ unshiftMessage (Discard $ toTarget a)
    AssetDamage aid _ health sanity | aid == assetId ->
      pure $ a & healthDamageL +~ health & sanityDamageL +~ sanity
    When (InvestigatorResigned iid) | assetInvestigator == Just iid ->
      a <$ unshiftMessage (ResignWith (AssetTarget assetId))
    InvestigatorEliminated iid | assetInvestigator == Just iid ->
      a <$ unshiftMessage (Discard (AssetTarget assetId))
    AddUses target useType' n | a `isTarget` target -> case assetUses of
      Uses useType'' m | useType' == useType'' ->
        pure $ a & usesL .~ Uses useType' (n + m)
      _ -> error "Trying to add the wrong use type"
    SpendUses target useType' n | isTarget a target -> case assetUses of
      Uses useType'' m | useType' == useType'' ->
        pure $ a & usesL .~ Uses useType' (max 0 (m - n))
      _ -> error "Trying to use the wrong use type"
    AttachAsset aid target | aid == assetId -> case target of
      LocationTarget lid ->
        pure
          $ a
          & investigatorL
          .~ Nothing
          & enemyL
          .~ Nothing
          & locationL
          ?~ lid
      EnemyTarget eid ->
        pure
          $ a
          & investigatorL
          .~ Nothing
          & locationL
          .~ Nothing
          & enemyL
          ?~ eid
      _ -> error "Cannot attach asset to that type"
    RemoveFromGame target | a `isTarget` target ->
      a <$ unshiftMessage (RemovedFromPlay $ toSource a)
    Discard target | a `isTarget` target -> a <$ unshiftMessages
      [RemovedFromPlay $ toSource a, Discarded target (toCard a)]
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage $ CheckWindow iid [WhenEnterPlay $ toTarget a]
      pure $ a & investigatorL ?~ iid
    TakeControlOfAsset iid aid | aid == assetId ->
      pure $ a & investigatorL ?~ iid
    AddToScenarioDeck target | isTarget a target -> do
      unshiftMessages
        [AddCardToScenarioDeck (toCard a), RemoveFromGame (toTarget a)]
      pure $ a & investigatorL .~ Nothing
    Exhaust target | a `isTarget` target -> pure $ a & exhaustedL .~ True
    Ready target | a `isTarget` target -> case assetInvestigator of
      Just iid -> do
        modifiers <-
          map modifierType
            <$> getModifiersFor (toSource a) (InvestigatorTarget iid) ()
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhaustedL .~ False
      Nothing -> pure $ a & exhaustedL .~ False
    Blanked msg' -> runMessage msg' a
    _ -> pure a
