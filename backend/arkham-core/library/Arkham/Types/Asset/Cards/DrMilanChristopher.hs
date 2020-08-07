{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DrMilanChristopher where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude


newtype DrMilanChristopherI = DrMilanChristopherI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drMilanChristopher :: AssetId -> DrMilanChristopherI
drMilanChristopher uuid = DrMilanChristopherI $ (baseAttrs uuid "01033")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance (AssetRunner env) => RunMessage env DrMilanChristopherI where
  runMessage msg a@(DrMilanChristopherI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (AddModifier
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 1 (AssetSource aid))
        )
      DrMilanChristopherI <$> runMessage msg attrs
    SuccessfulInvestigation iid _ | iid == getInvestigator attrs ->
      a <$ unshiftMessage
        (Ask $ ChooseOne
          [ UseCardAbility
            iid
            (AssetSource assetId, Nothing, 1, ReactionAbility Fast.Now, NoLimit)
          , Continue "Do not use Dr. Christopher Milan's ability"
          ]
        )
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> DrMilanChristopherI <$> runMessage msg attrs
