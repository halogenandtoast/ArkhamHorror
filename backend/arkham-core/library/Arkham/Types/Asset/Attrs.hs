{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Attrs where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import GHC.Stack
import Lens.Micro
import Lens.Micro.Extras
import Safe (fromJustNote)

data Attrs = Attrs
  { assetName :: Text
  , assetId :: AssetId
  , assetCardCode :: CardCode
  , assetCost :: Int
  , assetInvestigator :: Maybe InvestigatorId
  , assetActions :: [Message]
  , assetSlots :: [SlotType]
  , assetHealth :: Maybe Int
  , assetSanity :: Maybe Int
  , assetHealthDamage :: Int
  , assetSanityDamage :: Int
  , assetTraits :: HashSet Trait
  , assetAbilities :: [Ability]
  , assetUses :: Uses
  , assetExhausted :: Bool
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
    MkPlayerCard {..} =
      fromJustNote
          "missing player card"
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unAssetId aid)
  in
    Attrs
      { assetName = pcName
      , assetId = aid
      , assetCardCode = cardCode
      , assetCost = pcCost
      , assetInvestigator = Nothing
      , assetActions = mempty
      , assetSlots = mempty
      , assetHealth = Nothing
      , assetSanity = Nothing
      , assetHealthDamage = 0
      , assetSanityDamage = 0
      , assetTraits = HashSet.fromList pcTraits
      , assetAbilities = mempty
      , assetUses = NoUses
      , assetExhausted = False
      }

exhausted :: Lens' Attrs Bool
exhausted = lens assetExhausted $ \m x -> m { assetExhausted = x }

uses :: Lens' Attrs Uses
uses = lens assetUses $ \m x -> m { assetUses = x }

investigator :: Lens' Attrs (Maybe InvestigatorId)
investigator = lens assetInvestigator $ \m x -> m { assetInvestigator = x }

getInvestigator :: HasCallStack => Attrs -> InvestigatorId
getInvestigator = fromJustNote "asset must be owned" . view investigator

healthDamage :: Lens' Attrs Int
healthDamage = lens assetHealthDamage $ \m x -> m { assetHealthDamage = x }

sanityDamage :: Lens' Attrs Int
sanityDamage = lens assetSanityDamage $ \m x -> m { assetSanityDamage = x }

abilities :: Lens' Attrs [Ability]
abilities = lens assetAbilities $ \m x -> m { assetAbilities = x }

defeated :: Attrs -> Bool
defeated Attrs {..} =
  maybe False (assetHealthDamage >=) assetHealth
    || maybe False (assetSanityDamage >=) assetSanity

instance (AssetRunner env) => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    ReadyExhausted -> pure $ a & exhausted .~ False
    AddAbility (AssetSource aid) ability | aid == assetId ->
      pure $ a & abilities %~ (<> [ability])
    RemoveAbilitiesFrom source -> do
      let
        abilities' = filter
          (\(_, source', _, _, _) -> Just source /= source')
          assetAbilities
      pure $ a & abilities .~ abilities'
    CheckDefeated ->
      a <$ when (defeated a) (unshiftMessage (AssetDefeated assetId))
    AssetDamage aid _ health sanity | aid == assetId ->
      pure $ a & healthDamage +~ health & sanityDamage +~ sanity
    DiscardAsset aid | aid == assetId -> case assetInvestigator of
      Nothing -> pure a
      Just iid -> a <$ unshiftMessage
        (RemoveAllModifiersOnTargetFrom
          (InvestigatorTarget iid)
          (AssetSource aid)
        )
    InvestigatorPlayAsset iid aid _ _ | aid == assetId ->
      pure $ a & investigator ?~ iid
    TakeControlOfAsset iid aid | aid == assetId ->
      pure $ a & investigator ?~ iid
    _ -> pure a
