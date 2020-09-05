{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.TheNecronomicon where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token
import Arkham.Types.Window
import ClassyPrelude

newtype TheNecronomiconMetadata = TheNecronomiconMetadata { theNecronomiconHorror :: Int }
  deriving stock (Show, Generic)

instance ToJSON TheNecronomiconMetadata where
  toJSON = genericToJSON $ aesonOptions $ Just "theNecronomicon"
  toEncoding = genericToEncoding $ aesonOptions $ Just "theNecronomicon"

instance FromJSON TheNecronomiconMetadata where
  parseJSON = genericParseJSON $ aesonOptions $ Just "theNecronomicon"

newtype TheNecronomicon = TheNecronomicon (Attrs `With` TheNecronomiconMetadata)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

theNecronomicon :: AssetId -> TheNecronomicon
theNecronomicon uuid =
  TheNecronomicon
    $ ((baseAttrs uuid "01009") { assetSlots = [HandSlot] })
    `with` TheNecronomiconMetadata 3

instance (IsInvestigator investigator) => HasActions env investigator TheNecronomicon where
  getActions i NonFast (TheNecronomicon (Attrs {..} `With` TheNecronomiconMetadata {..}))
    | Just (getId () i) == assetInvestigator
    = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      | theNecronomiconHorror > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env TheNecronomicon where
  runMessage msg a@(TheNecronomicon (attrs@Attrs {..} `With` metadata@TheNecronomiconMetadata {..}))
    = case msg of
      InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
        unshiftMessage
          (AddModifiers
            (InvestigatorTarget iid)
            (AssetSource aid)
            [ForcedTokenChange Token.ElderSign Token.AutoFail]
          )
        TheNecronomicon . (`with` metadata) <$> runMessage msg attrs
      UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId -> do
        unshiftMessage $ InvestigatorDamage iid (AssetSource aid) 0 1
        if theNecronomiconHorror == 1
          then a <$ unshiftMessage (Discard (AssetTarget aid))
          else pure $ TheNecronomicon
            (attrs `with` TheNecronomiconMetadata (theNecronomiconHorror - 1))
      _ -> TheNecronomicon . (`with` metadata) <$> runMessage msg attrs
