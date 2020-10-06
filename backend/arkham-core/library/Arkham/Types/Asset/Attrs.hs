{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Attrs where

import Arkham.Json
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (playerCardAttrs)
import qualified Arkham.Types.Card.PlayerCard.Attrs as PlayerCard
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import GHC.Stack
import Lens.Micro
import Lens.Micro.Extras
import Safe (fromJustNote)

data Attrs = Attrs
  { assetName :: Text
  , assetId :: AssetId
  , assetCardCode :: CardCode
  , assetCost :: CardCost
  , assetInvestigator :: Maybe InvestigatorId
  , assetLocation :: Maybe LocationId
  , assetActions :: [Message]
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetHealthDamage :: Int
  , assetSanityDamage :: Int
  , assetTraits :: HashSet Trait
  , assetUses :: Uses
  , assetExhausted :: Bool
  , assetDoom :: Int
  , assetHorror :: Maybe Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "asset"
  toEncoding = genericToEncoding $ aesonOptions $ Just "asset"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "asset"

baseAttrs :: AssetId -> CardCode -> Attrs
baseAttrs aid cardCode =
  let
    PlayerCard.Attrs {..} =
      playerCardAttrs
        . fromJustNote "missing player card" (lookup cardCode allPlayerCards)
        $ CardId (unAssetId aid)
  in
    Attrs
      { assetName = pcName
      , assetId = aid
      , assetCardCode = cardCode
      , assetCost = pcCost
      , assetInvestigator = Nothing
      , assetLocation = Nothing
      , assetActions = mempty
      , assetSlots = mempty
      , assetHealth = Nothing
      , assetSanity = Nothing
      , assetHealthDamage = 0
      , assetSanityDamage = 0
      , assetTraits = setFromList pcTraits
      , assetUses = NoUses
      , assetExhausted = False
      , assetDoom = 0
      , assetHorror = Nothing
      }

doom :: Lens' Attrs Int
doom = lens assetDoom $ \m x -> m { assetDoom = x }

exhausted :: Lens' Attrs Bool
exhausted = lens assetExhausted $ \m x -> m { assetExhausted = x }

uses :: Lens' Attrs Uses
uses = lens assetUses $ \m x -> m { assetUses = x }

investigator :: Lens' Attrs (Maybe InvestigatorId)
investigator = lens assetInvestigator $ \m x -> m { assetInvestigator = x }

location :: Lens' Attrs (Maybe LocationId)
location = lens assetLocation $ \m x -> m { assetLocation = x }

getInvestigator :: HasCallStack => Attrs -> InvestigatorId
getInvestigator = fromJustNote "asset must be owned" . view investigator

healthDamage :: Lens' Attrs Int
healthDamage = lens assetHealthDamage $ \m x -> m { assetHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage = lens assetSanityDamage $ \m x -> m { assetSanityDamage = x }

defeated :: Attrs -> Bool
defeated Attrs {..} =
  maybe False (assetHealthDamage >=) assetHealth
    || maybe False (assetSanityDamage >=) assetSanity

instance HasActions env investigator Attrs where
  getActions _ _ _ = pure []

is :: Target -> Attrs -> Bool
is (AssetTarget aid) a = aid == assetId a
is (CardCodeTarget cardCode) a = cardCode == assetCardCode a
is (CardIdTarget cardId) a = unCardId cardId == unAssetId (assetId a)
is _ _ = False

instance (AssetRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    ReadyExhausted -> case assetInvestigator of
      Just iid -> do
        modifiers <- getModifiers iid
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhausted .~ False
      Nothing -> pure $ a & exhausted .~ False
    CheckDefeated ->
      a <$ when (defeated a) (unshiftMessage (AssetDefeated assetId))
    AssetDamage aid _ health sanity | aid == assetId ->
      pure $ a & healthDamage +~ health & sanityDamage +~ sanity
    InvestigatorEliminated iid | assetInvestigator == Just iid ->
      a <$ unshiftMessage (Discard (AssetTarget assetId))
    AddUses target useType n | target `is` a -> case assetUses of
      Uses useType' m | useType == useType' ->
        pure $ a & uses .~ Uses useType (n + m)
      _ -> error "Trying to add the wrong use type"
    AddAssetAt aid lid | aid == assetId -> pure $ a & location ?~ lid
    Discard target | target `is` a -> case assetInvestigator of
      Nothing -> pure a
      Just iid -> a <$ unshiftMessage
        (RemoveAllModifiersOnTargetFrom
          (InvestigatorTarget iid)
          (AssetSource assetId)
        )
    InvestigatorPlayAsset iid aid _ _ | aid == assetId ->
      pure $ a & investigator ?~ iid
    TakeControlOfAsset iid aid | aid == assetId ->
      pure $ a & investigator ?~ iid
    Exhaust target | target `is` a -> pure $ a & exhausted .~ True
    Ready target | target `is` a -> case assetInvestigator of
      Just iid -> do
        modifiers <- getModifiers iid
        if ControlledAssetsCannotReady `elem` modifiers
          then pure a
          else pure $ a & exhausted .~ False
      Nothing -> pure $ a & exhausted .~ False
    _ -> pure a
