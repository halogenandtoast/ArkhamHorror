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
    $ ((baseAttrs uuid "01009")
        { assetSlots = [HandSlot]
        , assetAbilities =
          [ ( AssetSource uuid
            , AssetSource uuid
            , 1
            , ActionAbility 1 Nothing
            , NoLimit
            )
          ]
        }
      )
    `with` TheNecronomiconMetadata 3


instance (AssetRunner env) => RunMessage env TheNecronomicon where
  runMessage msg a@(TheNecronomicon (attrs@Attrs {..} `With` metadata@TheNecronomiconMetadata {..}))
    = case msg of
      InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
        unshiftMessage
          (AddModifier
            (InvestigatorTarget iid)
            (ForcedTokenChange Token.ElderSign Token.AutoFail (AssetSource aid))
          )
        TheNecronomicon . (`with` metadata) <$> runMessage msg attrs
      UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId -> do
        unshiftMessage $ InvestigatorDamage iid (AssetSource aid) 0 1
        if theNecronomiconHorror == 1
          then a <$ unshiftMessage (DiscardAsset aid)
          else pure $ TheNecronomicon
            (attrs `with` TheNecronomiconMetadata (theNecronomiconHorror - 1))
      _ -> TheNecronomicon . (`with` metadata) <$> runMessage msg attrs
