module Arkham.Asset.Attrs
  ( module X
  , module Arkham.Asset.Attrs
  ) where

import Arkham.Prelude

import Arkham.Asset.Helpers as X
import Arkham.Asset.Runner as X
import Arkham.Asset.Uses as X
import Arkham.Classes as X
import Arkham.Message as X

import Arkham.Ability
import Arkham.Asset.Cards
import Arkham.Card
import Arkham.Json
import Arkham.Card
import Arkham.Id
import Arkham.Modifier
import Arkham.Name
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

class IsAsset a

type AssetCard a = CardBuilder AssetId a

data AssetAttrs = AssetAttrs
  { assetId :: AssetId
  , assetCardCode :: CardCode
  , assetOriginalCardCode :: CardCode
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
  , assetDiscardWhenNoUses :: Bool
  , assetIsStory :: Bool
  }
  deriving stock (Show, Eq, Generic)

discardWhenNoUsesL :: Lens' AssetAttrs Bool
discardWhenNoUsesL =
  lens assetDiscardWhenNoUses $ \m x -> m { assetDiscardWhenNoUses = x }

canLeavePlayByNormalMeansL :: Lens' AssetAttrs Bool
canLeavePlayByNormalMeansL = lens assetCanLeavePlayByNormalMeans
  $ \m x -> m { assetCanLeavePlayByNormalMeans = x }

horrorL :: Lens' AssetAttrs (Maybe Int)
horrorL = lens assetHorror $ \m x -> m { assetHorror = x }

isStoryL :: Lens' AssetAttrs Bool
isStoryL = lens assetIsStory $ \m x -> m { assetIsStory = x }

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

healthDamageL :: Lens' AssetAttrs Int
healthDamageL = lens assetHealthDamage $ \m x -> m { assetHealthDamage = x }

sanityDamageL :: Lens' AssetAttrs Int
sanityDamageL = lens assetSanityDamage $ \m x -> m { assetSanityDamage = x }

usesL :: Lens' AssetAttrs Uses
usesL = lens assetUses $ \m x -> m { assetUses = x }

locationL :: Lens' AssetAttrs (Maybe LocationId)
locationL = lens assetLocation $ \m x -> m { assetLocation = x }

enemyL :: Lens' AssetAttrs (Maybe EnemyId)
enemyL = lens assetEnemy $ \m x -> m { assetEnemy = x }

investigatorL :: Lens' AssetAttrs (Maybe InvestigatorId)
investigatorL = lens assetInvestigator $ \m x -> m { assetInvestigator = x }

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

allyWith
  :: (AssetAttrs -> a)
  -> CardDef
  -> (Int, Int)
  -> (AssetAttrs -> AssetAttrs)
  -> CardBuilder AssetId a
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
  -> CardBuilder AssetId a
assetWith f cardDef g = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \aid -> f . g $ AssetAttrs
    { assetId = aid
    , assetCardCode = toCardCode cardDef
    , assetOriginalCardCode = toCardCode cardDef
    , assetInvestigator = Nothing
    , assetLocation = Nothing
    , assetEnemy = Nothing
    , assetActions = mempty
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
  isSource AssetAttrs { assetId } (AssetSource aid) = assetId == aid
  isSource _ _ = False

ownedBy :: AssetAttrs -> InvestigatorId -> Bool
ownedBy AssetAttrs {..} = (== assetInvestigator) . Just

whenOwnedBy
  :: Applicative m => AssetAttrs -> InvestigatorId -> m [Ability] -> m [Ability]
whenOwnedBy a iid f = if ownedBy a iid then f else pure []

getInvestigator :: HasCallStack => AssetAttrs -> InvestigatorId
getInvestigator = fromJustNote "asset must be owned" . view investigatorL

defeated :: AssetAttrs -> Bool
defeated AssetAttrs {..} =
  maybe False (assetHealthDamage >=) assetHealth
    || maybe False (assetSanityDamage >=) assetSanity

instance AssetRunner env => RunMessage env AssetAttrs where
  runMessage msg a@AssetAttrs {..} = case msg of
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    ReadyExhausted -> case assetInvestigator of
      Just iid -> do
        modifiers <- getModifiers (toSource a) (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else a <$ push (Ready $ toTarget a)
      Nothing -> a <$ push (Ready $ toTarget a)
    RemoveAllDoom -> pure $ a & doomL .~ 0
    PlaceClues target n | isTarget a target -> pure $ a & cluesL +~ n
    PlaceDoom target n | isTarget a target -> pure $ a & doomL +~ n
    RemoveDoom target n | isTarget a target ->
      pure $ a & doomL %~ min 0 . subtract n
    RemoveClues target n | isTarget a target -> do
      when (assetClues - n <= 0) $ pushAll =<< windows
        [Window.LastClueRemovedFromAsset (toId a)]
      pure $ a & cluesL %~ max 0 . subtract n
    CheckDefeated _ -> do
      when (defeated a) $ do
        whenWindow <- checkWindows
          [Window Timing.When (Window.AssetDefeated $ toId a)]
        afterWindow <- checkWindows
          [Window Timing.When (Window.AssetDefeated $ toId a)]
        pushAll
          $ [whenWindow]
          <> resolve (AssetDefeated assetId)
          <> [afterWindow]
      pure a
    AssetDefeated aid | aid == assetId -> a <$ push (Discard $ toTarget a)
    AssetDamage aid _ health sanity | aid == assetId ->
      pure $ a & healthDamageL +~ health & sanityDamageL +~ sanity
    When (InvestigatorResigned iid) | assetInvestigator == Just iid ->
      a <$ push (ResignWith (AssetTarget assetId))
    InvestigatorEliminated iid | assetInvestigator == Just iid ->
      a <$ push (Discard (AssetTarget assetId))
    AddUses target useType' n | a `isTarget` target -> case assetUses of
      Uses useType'' m | useType' == useType'' ->
        pure $ a & usesL .~ Uses useType' (n + m)
      _ -> error "Trying to add the wrong use type"
    SpendUses target useType' n | isTarget a target -> case assetUses of
      Uses useType'' m | useType' == useType'' -> do
        let remainingUses = max 0 (m - n)
        when
          (assetDiscardWhenNoUses && remainingUses == 0)
          (push $ Discard $ toTarget a)
        pure $ a & usesL .~ Uses useType' remainingUses
      _ -> error "Trying to use the wrong use type"
    AttachAsset aid target | aid == assetId -> case target of
      LocationTarget lid ->
        pure
          $ a
          & (investigatorL .~ Nothing)
          . (enemyL .~ Nothing)
          . (locationL ?~ lid)
      EnemyTarget eid ->
        pure
          $ a
          & (investigatorL .~ Nothing)
          . (locationL .~ Nothing)
          . (enemyL ?~ eid)
      _ -> error "Cannot attach asset to that type"
    RemoveFromGame target | a `isTarget` target ->
      a <$ push (RemovedFromPlay $ toSource a)
    Discard target | a `isTarget` target -> do
      windows' <- windows [Window.WouldBeDiscarded (toTarget a)]
      a <$ pushAll
        (windows'
        <> [RemovedFromPlay $ toSource a, Discarded (toTarget a) (toCard a)]
        )
    Exile target | a `isTarget` target ->
      a <$ pushAll [RemovedFromPlay $ toSource a, Exiled target (toCard a)]
    RemovedFromPlay source | isSource a source -> do
      push =<< checkWindows
        ((`Window` Window.LeavePlay (toTarget a))
        <$> [Timing.When, Timing.After]
        )
      pure a
    InvestigatorPlayedAsset iid aid _ _ | aid == assetId -> do
      -- we specifically use the investigator source here because the
      -- asset has no knowledge of being owned yet, and this will allow
      -- us to bring the investigator's id into scope
      modifiers <- getModifiers (InvestigatorSource iid) (toTarget a)
      let
        startingUses = cdUses $ toCardDef a
        applyModifier (Uses uType m) (AdditionalStartingUses n) =
          Uses uType (n + m)
        applyModifier m _ = m
      whenEnterMsg <- checkWindows
        [Window Timing.When (Window.EnterPlay $ toTarget a)]
      afterEnterMsg <- checkWindows
        [Window Timing.After (Window.EnterPlay $ toTarget a)]

      pushAll [whenEnterMsg, afterEnterMsg]
      pure
        $ a
        & (investigatorL ?~ iid)
        & (usesL .~ if assetUses == NoUses
            then foldl' applyModifier startingUses modifiers
            else assetUses
          )
    InvestigatorPlayDynamicAsset iid aid slots traits _ | aid == assetId ->
      a <$ push (InvestigatorPlayAsset iid aid slots traits)
    TakeControlOfAsset iid aid | aid == assetId -> do
      push =<< checkWindows
        ((`Window` Window.TookControlOfAsset iid aid)
        <$> [Timing.When, Timing.After]
        )
      pure $ a & investigatorL ?~ iid
    ReplacedInvestigatorAsset iid aid | aid == assetId ->
      pure $ a & investigatorL ?~ iid
    AddToScenarioDeck key target | isTarget a target -> do
      pushAll
        [AddCardToScenarioDeck key (toCard a), RemoveFromGame (toTarget a)]
      pure $ a & investigatorL .~ Nothing
    Exhaust target | a `isTarget` target -> pure $ a & exhaustedL .~ True
    Ready target | a `isTarget` target -> case assetInvestigator of
      Just iid -> do
        modifiers <- getModifiers (toSource a) (InvestigatorTarget iid)
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhaustedL .~ False
      Nothing -> pure $ a & exhaustedL .~ False
    Blanked msg' -> runMessage msg' a
    _ -> pure a
