{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.CryptChill where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype CryptChill = CryptChill Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cryptChill :: TreacheryId -> a -> CryptChill
cryptChill uuid _ = CryptChill $ baseAttrs uuid "01167"

instance HasActions env investigator CryptChill where
  getActions i window (CryptChill attrs) = getActions i window attrs

instance (TreacheryRunner env) => RunMessage env CryptChill where
  runMessage msg t@(CryptChill attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ RevelationSkillTest
          iid
          (TreacherySource tid)
          SkillWillpower
          4
          []
          [TreacheryFailure iid tid]
        , Discard (TreacheryTarget tid)
        ]
      CryptChill <$> runMessage msg (attrs & resolved .~ True)
    TreacheryFailure iid tid | tid == treacheryId -> do
      assetCount <- HashSet.size <$> asks (getSet @AssetId iid)
      if assetCount > 0
        then t <$ unshiftMessage (ChooseAndDiscardAsset iid)
        else t <$ unshiftMessage
          (InvestigatorAssignDamage iid (TreacherySource treacheryId) 2 0)
    _ -> CryptChill <$> runMessage msg attrs
