{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ResearchLibrarian where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype ResearchLibrarian = ResearchLibrarian Attrs
  deriving newtype (Show, ToJSON, FromJSON)

researchLibrarian :: AssetId -> ResearchLibrarian
researchLibrarian uuid = ResearchLibrarian $ baseAttrs uuid "01032" $ do
  slots .= [AllySlot]
  health ?= 1
  sanity ?= 1

instance HasModifiersFor env ResearchLibrarian where
  getModifiersFor _ _ _ = pure []

instance HasActions env ResearchLibrarian where
  getActions i window (ResearchLibrarian x) = getActions i window x

instance (AssetRunner env) => RunMessage env ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage
        (chooseOne
          iid
          [ UseCardAbility iid (toSource attrs) Nothing 1
          , Continue "Do not use ability"
          ]
        )
      ResearchLibrarian <$> runMessage msg attrs
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Tome])
    _ -> ResearchLibrarian <$> runMessage msg attrs
