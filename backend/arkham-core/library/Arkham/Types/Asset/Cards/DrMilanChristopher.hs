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
import ClassyPrelude


newtype DrMilanChristopher = DrMilanChristopher Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drMilanChristopher :: AssetId -> DrMilanChristopher
drMilanChristopher uuid = DrMilanChristopher $ (baseAttrs uuid "01033")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator DrMilanChristopher where
  getModifiersFor _ i (DrMilanChristopher Attrs {..}) =
    pure
      [ SkillModifier SkillIntellect 1
      | Just (getId () i) == assetInvestigator
      ]

instance HasActions env investigator DrMilanChristopher where
  getActions i window (DrMilanChristopher x) = getActions i window x

instance (AssetRunner env) => RunMessage env DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs@Attrs {..}) = case msg of
    SuccessfulInvestigation iid _ | iid == getInvestigator attrs ->
      a <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ UseCardAbility
            iid
            (AssetSource assetId)
            Nothing
            1
          , Continue "Do not use Dr. Christopher Milan's ability"
          ]
        )
    UseCardAbility iid (AssetSource aid) _ 1 | aid == assetId ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> DrMilanChristopher <$> runMessage msg attrs
