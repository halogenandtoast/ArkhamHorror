{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ResearchLibrarian where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import qualified Arkham.Types.FastWindow as Fast
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Trait
import ClassyPrelude

newtype ResearchLibrarianI = ResearchLibrarianI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

researchLibrarian :: AssetId -> ResearchLibrarianI
researchLibrarian uuid = ResearchLibrarianI $ (baseAttrs uuid "01032")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 1
  }

instance (AssetRunner env) => RunMessage env ResearchLibrarianI where
  runMessage msg a@(ResearchLibrarianI attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (Ask $ ChooseOne
          [ UseCardAbility
            iid
            (AssetSource assetId, Nothing, 1, ReactionAbility Fast.Now, NoLimit)
          , Continue "Do not use ability"
          ]
        )
      ResearchLibrarianI <$> runMessage msg attrs
    UseCardAbility iid (AssetSource aid, _, 1, _, _) | aid == assetId ->
      a <$ unshiftMessage (SearchDeckForTraits iid [Tome])
    _ -> ResearchLibrarianI <$> runMessage msg attrs
