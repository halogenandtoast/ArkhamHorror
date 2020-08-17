{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DrMilanChristopher where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude


newtype DrMilanChristopher = DrMilanChristopher Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drMilanChristopher :: AssetId -> DrMilanChristopher
drMilanChristopher uuid = DrMilanChristopher $ (baseAttrs uuid "01033")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance (AssetRunner env) => RunMessage env DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifier
          (InvestigatorTarget iid)
          (SkillModifier SkillIntellect 1 (AssetSource aid))
        )
      DrMilanChristopher <$> runMessage msg attrs
    SuccessfulInvestigation iid _ | iid == getInvestigator attrs ->
      a <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ UseCardAbility iid (AssetSource assetId) (AssetSource assetId) 1
          , Continue "Do not use Dr. Christopher Milan's ability"
          ]
        )
    UseCardAbility iid _ (AssetSource aid) 1 | aid == assetId ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> DrMilanChristopher <$> runMessage msg attrs
